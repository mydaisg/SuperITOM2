"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.WorkOrdersService = void 0;
const common_1 = require("@nestjs/common");
const typeorm_1 = require("@nestjs/typeorm");
const typeorm_2 = require("typeorm");
const work_order_entity_1 = require("./entities/work-order.entity");
const work_order_comment_entity_1 = require("./entities/work-order-comment.entity");
const user_entity_1 = require("../auth/entities/user.entity");
function formatDate(d) {
    if (!d)
        return '';
    const pad = (n) => String(n).padStart(2, '0');
    return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}
let WorkOrdersService = class WorkOrdersService {
    constructor(workOrderRepo, commentRepo, userRepo) {
        this.workOrderRepo = workOrderRepo;
        this.commentRepo = commentRepo;
        this.userRepo = userRepo;
    }
    canAssign(user) {
        return user.role === 'admin' || user.role === 'it_desk';
    }
    async buildNameMap(ids) {
        const uniqueIds = Array.from(new Set(ids.filter((id) => id !== null && id !== undefined)));
        const map = new Map();
        if (uniqueIds.length === 0)
            return map;
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
    async generateOrderNo() {
        const now = new Date();
        const ymd = `${now.getFullYear()}${String(now.getMonth() + 1).padStart(2, '0')}${String(now.getDate()).padStart(2, '0')}`;
        const prefix = `ITS${ymd}`;
        const count = await this.workOrderRepo
            .createQueryBuilder('wo')
            .where('wo.orderNo LIKE :prefix', { prefix: `${prefix}%` })
            .getCount();
        const seq = String(count + 1).padStart(3, '0');
        return `${prefix}${seq}`;
    }
    async list(user, status) {
        const qb = this.workOrderRepo.createQueryBuilder('wo');
        if (user.role !== 'admin') {
            qb.where('(wo.creatorId = :uid OR wo.assigneeId = :uid OR wo.handlerId = :uid)', { uid: user.id });
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
    async detail(id) {
        const wo = await this.workOrderRepo.findOne({ where: { id } });
        if (!wo) {
            throw new common_1.NotFoundException('工单不存在');
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
    async comments(id) {
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
    async addComment(user, id, dto) {
        await this.ensureExists(id);
        const comment = this.commentRepo.create({
            workOrderId: id,
            comment: dto.comment,
            creatorId: user.id,
        });
        await this.commentRepo.save(comment);
        return this.comments(id);
    }
    async create(user, dto) {
        const orderNo = await this.generateOrderNo();
        const wo = this.workOrderRepo.create({
            orderNo,
            title: dto.title,
            description: dto.description || '',
            priority: dto.priority || '中',
            category: dto.category || '一般',
            subcategory: dto.subcategory || '',
            requestUser: dto.requestUser || '',
            status: work_order_entity_1.WorkOrderStatus.PENDING,
            creatorId: user.id,
        });
        const saved = await this.workOrderRepo.save(wo);
        return this.detail(saved.id);
    }
    async assign(user, id, dto) {
        if (!this.canAssign(user)) {
            throw new common_1.ForbiddenException('无权派发工单');
        }
        const wo = await this.ensureExists(id);
        if (wo.status !== work_order_entity_1.WorkOrderStatus.PENDING) {
            throw new common_1.BadRequestException('只有待处理的工单才能派发');
        }
        await this.workOrderRepo.update(id, {
            assigneeId: dto.assigneeId,
            status: work_order_entity_1.WorkOrderStatus.ASSIGNED,
            assignedAt: new Date(),
        });
        return this.detail(id);
    }
    async start(user, id) {
        const wo = await this.ensureExists(id);
        if (wo.status !== work_order_entity_1.WorkOrderStatus.ASSIGNED) {
            throw new common_1.BadRequestException('只有已派发的工单才能开始处理');
        }
        if (wo.assigneeId !== user.id) {
            throw new common_1.ForbiddenException('只有指派人才能开始处理');
        }
        await this.workOrderRepo.update(id, {
            handlerId: user.id,
            status: work_order_entity_1.WorkOrderStatus.PROCESSING,
        });
        return this.detail(id);
    }
    async complete(user, id, dto) {
        const wo = await this.ensureExists(id);
        if (wo.status !== work_order_entity_1.WorkOrderStatus.PROCESSING) {
            throw new common_1.BadRequestException('只有处理中的工单才能完成');
        }
        if (wo.handlerId !== user.id) {
            throw new common_1.ForbiddenException('只有处理人才能完成工单');
        }
        await this.workOrderRepo.update(id, {
            status: work_order_entity_1.WorkOrderStatus.COMPLETED,
            resolution: dto.resolution || '',
            completedAt: new Date(),
        });
        return this.detail(id);
    }
    async close(user, id, dto) {
        const wo = await this.ensureExists(id);
        if (wo.status === work_order_entity_1.WorkOrderStatus.COMPLETED ||
            wo.status === work_order_entity_1.WorkOrderStatus.CLOSED) {
            throw new common_1.BadRequestException('工单已结束，无法关闭');
        }
        const isInvolved = wo.handlerId === user.id || wo.assigneeId === user.id || this.canAssign(user);
        if (!isInvolved) {
            throw new common_1.ForbiddenException('无权关闭该工单');
        }
        const reason = dto.reason || '';
        if (reason) {
            await this.workOrderRepo.update(id, {
                status: work_order_entity_1.WorkOrderStatus.CLOSED,
                resolution: `[已关闭] ${reason}`,
                completedAt: new Date(),
                closedAt: new Date(),
                closeReason: reason,
            });
        }
        else {
            await this.workOrderRepo.update(id, {
                status: work_order_entity_1.WorkOrderStatus.CLOSED,
                closedAt: new Date(),
            });
        }
        return this.detail(id);
    }
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
    async ensureExists(id) {
        const wo = await this.workOrderRepo.findOne({ where: { id } });
        if (!wo) {
            throw new common_1.NotFoundException('工单不存在');
        }
        return wo;
    }
};
exports.WorkOrdersService = WorkOrdersService;
exports.WorkOrdersService = WorkOrdersService = __decorate([
    (0, common_1.Injectable)(),
    __param(0, (0, typeorm_1.InjectRepository)(work_order_entity_1.WorkOrderEntity)),
    __param(1, (0, typeorm_1.InjectRepository)(work_order_comment_entity_1.WorkOrderCommentEntity)),
    __param(2, (0, typeorm_1.InjectRepository)(user_entity_1.UserEntity)),
    __metadata("design:paramtypes", [typeorm_2.Repository,
        typeorm_2.Repository,
        typeorm_2.Repository])
], WorkOrdersService);
//# sourceMappingURL=work-orders.service.js.map