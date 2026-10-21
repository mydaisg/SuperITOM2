import {
  Injectable,
  NotFoundException,
  ForbiddenException,
  BadRequestException,
} from '@nestjs/common';
import { InjectRepository } from '@nestjs/typeorm';
import { Repository } from 'typeorm';
import { WorkOrderEntity, WorkOrderStatus } from './entities/work-order.entity';
import { WorkOrderCommentEntity } from './entities/work-order-comment.entity';
import { UserEntity } from '../auth/entities/user.entity';
import { CreateWorkOrderDto } from './dto/create-work-order.dto';
import { AssignWorkOrderDto } from './dto/assign-work-order.dto';
import { CompleteWorkOrderDto } from './dto/complete-work-order.dto';
import { CloseWorkOrderDto } from './dto/close-work-order.dto';
import { AddCommentDto } from './dto/add-comment.dto';

export interface AuthUser {
  id: number;
  username: string;
  role: string;
  displayName?: string;
  [key: string]: unknown;
}

function formatDate(d: Date | null | undefined): string {
  if (!d) return '';
  const pad = (n: number) => String(n).padStart(2, '0');
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(
    d.getHours(),
  )}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}

@Injectable()
export class WorkOrdersService {
  constructor(
    @InjectRepository(WorkOrderEntity)
    private readonly workOrderRepo: Repository<WorkOrderEntity>,
    @InjectRepository(WorkOrderCommentEntity)
    private readonly commentRepo: Repository<WorkOrderCommentEntity>,
    @InjectRepository(UserEntity)
    private readonly userRepo: Repository<UserEntity>,
  ) {}

  private canAssign(user: AuthUser): boolean {
    return user.role === 'admin' || user.role === 'it_desk';
  }

  private async buildNameMap(
    ids: (number | null | undefined)[],
  ): Promise<Map<number, string>> {
    const uniqueIds = Array.from(
      new Set(ids.filter((id): id is number => id !== null && id !== undefined)),
    );
    const map = new Map<number, string>();
    if (uniqueIds.length === 0) return map;

    const users = await this.userRepo
      .createQueryBuilder('u')
      .where('u.id IN (:...ids)', { ids: uniqueIds })
      .getMany();

    for (const u of users) {
      const name = u.displayName && u.displayName.trim() ? u.displayName : u.username;
      map.set(u.id, name);
    }
    return map;
  }

  private async generateOrderNo(): Promise<string> {
    const now = new Date();
    const ymd = `${now.getFullYear()}${String(now.getMonth() + 1).padStart(2, '0')}${String(
      now.getDate(),
    ).padStart(2, '0')}`;
    const prefix = `ITS${ymd}`;
    const count = await this.workOrderRepo
      .createQueryBuilder('wo')
      .where('wo.orderNo LIKE :prefix', { prefix: `${prefix}%` })
      .getCount();
    const seq = String(count + 1).padStart(3, '0');
    return `${prefix}${seq}`;
  }

  // ── 列表 ──

  async list(user: AuthUser, status?: string) {
    const qb = this.workOrderRepo.createQueryBuilder('wo');

    if (user.role !== 'admin') {
      qb.where(
        '(wo.creatorId = :uid OR wo.assigneeId = :uid OR wo.handlerId = :uid)',
        { uid: user.id },
      );
    }

    if (status && status !== 'all') {
      qb.andWhere('wo.status = :status', { status });
    }

    const orders = await qb.orderBy('wo.updatedAt', 'DESC').getMany();

    const ids = orders.flatMap((o) => [o.creatorId, o.assigneeId, o.handlerId]);
    const nameMap = await this.buildNameMap(ids);

    return orders.map((o) => ({
      id: o.id,
      orderNo: o.orderNo,
      title: o.title,
      priority: o.priority,
      status: o.status,
      category: o.category,
      creatorName: nameMap.get(o.creatorId) || '',
      assigneeName: o.assigneeId ? nameMap.get(o.assigneeId) || '' : '',
      createdAt: formatDate(o.createdAt),
    }));
  }

  // ── 详情 ──

  async detail(id: number) {
    const wo = await this.workOrderRepo.findOne({ where: { id } });
    if (!wo) {
      throw new NotFoundException('工单不存在');
    }

    const nameMap = await this.buildNameMap([wo.creatorId, wo.assigneeId, wo.handlerId]);
    const commentCount = await this.commentRepo.count({ where: { workOrderId: id } });

    return {
      id: wo.id,
      orderNo: wo.orderNo,
      title: wo.title,
      description: wo.description,
      priority: wo.priority,
      status: wo.status,
      category: wo.category,
      subcategory: wo.subcategory,
      requestUser: wo.requestUser,
      resolution: wo.resolution,
      closeReason: wo.closeReason,
      creatorId: wo.creatorId,
      assigneeId: wo.assigneeId,
      handlerId: wo.handlerId,
      creatorName: nameMap.get(wo.creatorId) || '',
      assigneeName: wo.assigneeId ? nameMap.get(wo.assigneeId) || '' : '',
      handlerName: wo.handlerId ? nameMap.get(wo.handlerId) || '' : '',
      createdAt: formatDate(wo.createdAt),
      assignedAt: formatDate(wo.assignedAt),
      completedAt: formatDate(wo.completedAt),
      closedAt: formatDate(wo.closedAt),
      commentCount,
    };
  }

  // ── 评论 ──

  async comments(id: number) {
    await this.ensureExists(id);
    const rows = await this.commentRepo.find({
      where: { workOrderId: id },
      order: { createdAt: 'ASC', id: 'ASC' },
    });
    const nameMap = await this.buildNameMap(rows.map((c) => c.creatorId));
    return rows.map((c) => ({
      id: c.id,
      comment: c.comment,
      createdAt: formatDate(c.createdAt),
      creatorName: nameMap.get(c.creatorId) || '',
    }));
  }

  async addComment(user: AuthUser, id: number, dto: AddCommentDto) {
    await this.ensureExists(id);
    const comment = this.commentRepo.create({
      workOrderId: id,
      comment: dto.comment,
      creatorId: user.id,
    });
    await this.commentRepo.save(comment);
    return this.comments(id);
  }

  // ── 创建 ──

  async create(user: AuthUser, dto: CreateWorkOrderDto) {
    const orderNo = await this.generateOrderNo();
    const wo = this.workOrderRepo.create({
      orderNo,
      title: dto.title,
      description: dto.description || '',
      priority: dto.priority || '中',
      category: dto.category || '一般',
      subcategory: dto.subcategory || '',
      requestUser: dto.requestUser || '',
      status: WorkOrderStatus.PENDING,
      creatorId: user.id,
    });
    const saved = await this.workOrderRepo.save(wo);
    return this.detail(saved.id);
  }

  // ── 操作 ──

  async assign(user: AuthUser, id: number, dto: AssignWorkOrderDto) {
    if (!this.canAssign(user)) {
      throw new ForbiddenException('无权派发工单');
    }
    const wo = await this.ensureExists(id);
    if (wo.status !== WorkOrderStatus.PENDING) {
      throw new BadRequestException('只有待处理的工单才能派发');
    }
    await this.workOrderRepo.update(id, {
      assigneeId: dto.assigneeId,
      status: WorkOrderStatus.ASSIGNED,
      assignedAt: new Date(),
    });
    return this.detail(id);
  }

  async start(user: AuthUser, id: number) {
    const wo = await this.ensureExists(id);
    if (wo.status !== WorkOrderStatus.ASSIGNED) {
      throw new BadRequestException('只有已派发的工单才能开始处理');
    }
    if (wo.assigneeId !== user.id) {
      throw new ForbiddenException('只有指派人才能开始处理');
    }
    await this.workOrderRepo.update(id, {
      handlerId: user.id,
      status: WorkOrderStatus.PROCESSING,
    });
    return this.detail(id);
  }

  async complete(user: AuthUser, id: number, dto: CompleteWorkOrderDto) {
    const wo = await this.ensureExists(id);
    if (wo.status !== WorkOrderStatus.PROCESSING) {
      throw new BadRequestException('只有处理中的工单才能完成');
    }
    if (wo.handlerId !== user.id) {
      throw new ForbiddenException('只有处理人才能完成工单');
    }
    await this.workOrderRepo.update(id, {
      status: WorkOrderStatus.COMPLETED,
      resolution: dto.resolution || '',
      completedAt: new Date(),
    });
    return this.detail(id);
  }

  async close(user: AuthUser, id: number, dto: CloseWorkOrderDto) {
    const wo = await this.ensureExists(id);
    if (
      wo.status === WorkOrderStatus.COMPLETED ||
      wo.status === WorkOrderStatus.CLOSED
    ) {
      throw new BadRequestException('工单已结束，无法关闭');
    }
    const isInvolved =
      wo.handlerId === user.id || wo.assigneeId === user.id || this.canAssign(user);
    if (!isInvolved) {
      throw new ForbiddenException('无权关闭该工单');
    }

    const reason = dto.reason || '';
    if (reason) {
      await this.workOrderRepo.update(id, {
        status: WorkOrderStatus.CLOSED,
        resolution: `[已关闭] ${reason}`,
        completedAt: new Date(),
        closedAt: new Date(),
        closeReason: reason,
      });
    } else {
      await this.workOrderRepo.update(id, {
        status: WorkOrderStatus.CLOSED,
        closedAt: new Date(),
      });
    }
    return this.detail(id);
  }

  // ── 可派发用户 ──

  async assignableUsers() {
    const users = await this.userRepo
      .createQueryBuilder('u')
      .where('u.isActive = :active', { active: true })
      .orderBy('u.role', 'ASC')
      .addOrderBy('u.username', 'ASC')
      .getMany();
    return users.map((u) => ({
      id: u.id,
      username: u.username,
      role: u.role,
      displayName: u.displayName && u.displayName.trim() ? u.displayName : u.username,
    }));
  }

  private async ensureExists(id: number): Promise<WorkOrderEntity> {
    const wo = await this.workOrderRepo.findOne({ where: { id } });
    if (!wo) {
      throw new NotFoundException('工单不存在');
    }
    return wo;
  }
}
