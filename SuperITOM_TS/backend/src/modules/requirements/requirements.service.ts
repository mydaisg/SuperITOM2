import { Injectable, NotFoundException, BadRequestException } from '@nestjs/common';
import { DatabaseService } from '../../common/database/database.service';

export interface Requirement {
  id: number;
  reqNo: string;
  title: string;
  description: string;
  status: string;
  priority: string;
  owner: string;
  startDate: string;
  dueDate: string;
  createdBy: number;
  createdAt: string;
  updatedAt: string;
}

export interface RequirementProgress {
  id: number;
  requirementId: number;
  dept: string;
  person: string;
  status: string;
  progressDate: string;
  content: string;
  sortOrder: number;
  parentId: number;
  createdAt: string;
}

interface ReqRow {
  id: number;
  req_no: string;
  title: string;
  description: string;
  status: string;
  priority: string;
  owner: string;
  start_date: string;
  due_date: string;
  created_by: number;
  created_at: string;
  updated_at: string;
}

interface ProgRow {
  id: number;
  requirement_id: number;
  dept: string;
  person: string;
  status: string;
  progress_date: string;
  content: string;
  sort_order: number;
  parent_id: number;
  created_at: string;
}

@Injectable()
export class RequirementsService {
  constructor(private readonly db: DatabaseService) {}

  private mapReq(r: ReqRow): Requirement {
    return {
      id: r.id,
      reqNo: r.req_no || '',
      title: r.title,
      description: r.description || '',
      status: r.status || '进行中',
      priority: r.priority || '中',
      owner: r.owner || '',
      startDate: r.start_date || '',
      dueDate: r.due_date || '',
      createdBy: r.created_by || 0,
      createdAt: r.created_at || '',
      updatedAt: r.updated_at || '',
    };
  }

  private mapProg(r: ProgRow): RequirementProgress {
    return {
      id: r.id,
      requirementId: r.requirement_id,
      dept: r.dept || '',
      person: r.person || '',
      status: r.status || '',
      progressDate: r.progress_date || '',
      content: r.content || '',
      sortOrder: r.sort_order || 0,
      parentId: r.parent_id || 0,
      createdAt: r.created_at || '',
    };
  }

  private generateReqNo(): string {
    const today = this.db.get<{ d: string }>(
      "SELECT strftime('%Y%m%d','now','localtime') AS d",
    )!.d;
    const prefix = `REQ${today}`;
    const row = this.db.get<{ req_no: string }>(
      'SELECT req_no FROM requirements WHERE req_no LIKE ? ORDER BY req_no DESC LIMIT 1',
      [`${prefix}%`],
    );
    let seq = 1;
    if (row && row.req_no && row.req_no.length > prefix.length) {
      const n = parseInt(row.req_no.slice(prefix.length), 10);
      if (!isNaN(n)) seq = n + 1;
    }
    return `${prefix}${String(seq).padStart(3, '0')}`;
  }

  list(status?: string, keyword?: string): Requirement[] {
    let sql = `SELECT id, COALESCE(req_no,'') AS req_no, title, COALESCE(description,'') AS description,
                      COALESCE(status,'进行中') AS status, COALESCE(priority,'中') AS priority,
                      COALESCE(owner,'') AS owner, COALESCE(start_date,'') AS start_date,
                      COALESCE(due_date,'') AS due_date, COALESCE(created_by,0) AS created_by,
                      COALESCE(created_at,'') AS created_at, COALESCE(updated_at,'') AS updated_at
               FROM requirements WHERE 1=1`;
    const params: any[] = [];
    if (status) {
      sql += ' AND status = ?';
      params.push(status);
    }
    if (keyword) {
      sql += ' AND (title LIKE ? OR description LIKE ?)';
      params.push(`%${keyword}%`, `%${keyword}%`);
    }
    sql += ' ORDER BY updated_at DESC, id DESC';
    const rows = this.db.query<ReqRow>(sql, params);
    return rows.map((r) => this.mapReq(r));
  }

  detail(id: number): Requirement {
    const row = this.db.get<ReqRow>(
      `SELECT id, COALESCE(req_no,'') AS req_no, title, COALESCE(description,'') AS description,
              COALESCE(status,'进行中') AS status, COALESCE(priority,'中') AS priority,
              COALESCE(owner,'') AS owner, COALESCE(start_date,'') AS start_date,
              COALESCE(due_date,'') AS due_date, COALESCE(created_by,0) AS created_by,
              COALESCE(created_at,'') AS created_at, COALESCE(updated_at,'') AS updated_at
       FROM requirements WHERE id = ?`,
      [id],
    );
    if (!row) {
      throw new NotFoundException('需求不存在');
    }
    return this.mapReq(row);
  }

  create(dto: {
    title?: string;
    description?: string;
    status?: string;
    priority?: string;
    owner?: string;
    startDate?: string;
    dueDate?: string;
  }, userId: number): Requirement {
    const title = (dto.title || '').trim();
    if (!title) {
      throw new BadRequestException('标题不能为空');
    }
    const reqNo = this.generateReqNo();
    const info = this.db.run(
      `INSERT INTO requirements (req_no, title, description, status, priority, owner, start_date, due_date, created_by, created_at, updated_at)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now','localtime'), datetime('now','localtime'))`,
      [
        reqNo,
        title,
        dto.description || '',
        dto.status || '进行中',
        dto.priority || '中',
        dto.owner || '',
        dto.startDate || '',
        dto.dueDate || '',
        userId,
      ],
    );
    return this.detail(Number(info.lastInsertRowid));
  }

  update(
    id: number,
    dto: {
      title?: string;
      description?: string;
      status?: string;
      priority?: string;
      owner?: string;
      startDate?: string;
      dueDate?: string;
    },
  ): Requirement {
    this.detail(id);
    const sets: string[] = ["updated_at = datetime('now','localtime')"];
    const params: any[] = [];
    if (dto.title !== undefined) {
      sets.push('title = ?');
      params.push(dto.title);
    }
    if (dto.description !== undefined) {
      sets.push('description = ?');
      params.push(dto.description);
    }
    if (dto.status !== undefined) {
      sets.push('status = ?');
      params.push(dto.status);
    }
    if (dto.priority !== undefined) {
      sets.push('priority = ?');
      params.push(dto.priority);
    }
    if (dto.owner !== undefined) {
      sets.push('owner = ?');
      params.push(dto.owner);
    }
    if (dto.startDate !== undefined) {
      sets.push('start_date = ?');
      params.push(dto.startDate);
    }
    if (dto.dueDate !== undefined) {
      sets.push('due_date = ?');
      params.push(dto.dueDate);
    }
    params.push(id);
    this.db.run(`UPDATE requirements SET ${sets.join(', ')} WHERE id = ?`, params);
    return this.detail(id);
  }

  delete(id: number): { message: string } {
    this.detail(id);
    this.db.run('DELETE FROM requirement_progress WHERE requirement_id = ?', [id]);
    this.db.run('DELETE FROM requirements WHERE id = ?', [id]);
    return { message: '已删除' };
  }

  progress(id: number): RequirementProgress[] {
    this.detail(id);
    const rows = this.db.query<ProgRow>(
      `SELECT id, requirement_id, COALESCE(dept,'') AS dept, COALESCE(person,'') AS person,
              COALESCE(status,'') AS status, COALESCE(progress_date,'') AS progress_date,
              COALESCE(content,'') AS content, COALESCE(sort_order,0) AS sort_order,
              COALESCE(parent_id,0) AS parent_id, COALESCE(created_at,'') AS created_at
       FROM requirement_progress WHERE requirement_id = ? ORDER BY sort_order ASC, id ASC`,
      [id],
    );
    return rows.map((r) => this.mapProg(r));
  }

  addProgress(
    id: number,
    dto: {
      dept?: string;
      person?: string;
      status?: string;
      progressDate?: string;
      content?: string;
    },
  ): RequirementProgress[] {
    this.detail(id);
    const content = (dto.content || '').trim();
    if (!content) {
      throw new BadRequestException('进度内容不能为空');
    }
    const maxRow = this.db.get<{ m: number }>(
      'SELECT COALESCE(MAX(sort_order),0) AS m FROM requirement_progress WHERE requirement_id = ?',
      [id],
    );
    this.db.run(
      `INSERT INTO requirement_progress (requirement_id, dept, person, status, progress_date, content, sort_order, created_at)
       VALUES (?, ?, ?, ?, ?, ?, ?, datetime('now','localtime'))`,
      [
        id,
        dto.dept || '',
        dto.person || '',
        dto.status || '',
        dto.progressDate || '',
        content,
        (maxRow?.m || 0) + 1,
      ],
    );
    return this.progress(id);
  }

  updateProgress(
    progressId: number,
    dto: {
      dept?: string;
      person?: string;
      status?: string;
      progressDate?: string;
      content?: string;
    },
  ): { message: string } {
    const row = this.db.get<{ id: number }>('SELECT id FROM requirement_progress WHERE id = ?', [
      progressId,
    ]);
    if (!row) {
      throw new NotFoundException('进度条目不存在');
    }
    const sets: string[] = [];
    const params: any[] = [];
    if (dto.dept !== undefined) {
      sets.push('dept = ?');
      params.push(dto.dept);
    }
    if (dto.person !== undefined) {
      sets.push('person = ?');
      params.push(dto.person);
    }
    if (dto.status !== undefined) {
      sets.push('status = ?');
      params.push(dto.status);
    }
    if (dto.progressDate !== undefined) {
      sets.push('progress_date = ?');
      params.push(dto.progressDate);
    }
    if (dto.content !== undefined) {
      sets.push('content = ?');
      params.push(dto.content);
    }
    if (sets.length === 0) {
      return { message: '无变更' };
    }
    params.push(progressId);
    this.db.run(`UPDATE requirement_progress SET ${sets.join(', ')} WHERE id = ?`, params);
    return { message: '已更新' };
  }

  deleteProgress(progressId: number): { message: string } {
    this.db.run('DELETE FROM requirement_progress WHERE id = ?', [progressId]);
    return { message: '已删除' };
  }
}
