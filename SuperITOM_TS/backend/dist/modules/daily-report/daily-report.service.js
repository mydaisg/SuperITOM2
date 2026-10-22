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
Object.defineProperty(exports, "__esModule", { value: true });
exports.DailyReportService = void 0;
const common_1 = require("@nestjs/common");
const database_service_1 = require("../../common/database/database.service");
let DailyReportService = class DailyReportService {
    constructor(db) {
        this.db = db;
    }
    validDate(date) {
        const d = (date || '').trim();
        if (!/^\d{4}-\d{2}-\d{2}$/.test(d)) {
            throw new common_1.BadRequestException('日期格式应为 YYYY-MM-DD');
        }
        return d;
    }
    getWorkOrders(date) {
        return this.db.query(`SELECT wo.id, COALESCE(wo.order_no,'') AS order_no, wo.title, COALESCE(wo.priority,'') AS priority,
              COALESCE(wo.status,'') AS status, COALESCE(wo.category,'') AS category,
              wo.assigned_to, wo.handled_by, wo.created_by,
              COALESCE(wo.created_at,'') AS created_at, COALESCE(wo.handled_at,'') AS handled_at,
              COALESCE(wo.completed_at,'') AS completed_at,
              COALESCE(NULLIF(u_assign.display_name,''), u_assign.username,'') AS assignee_name,
              COALESCE(NULLIF(u_handler.display_name,''), u_handler.username,'') AS handler_name,
              COALESCE(NULLIF(u_creator.display_name,''), u_creator.username,'') AS creator_name
       FROM work_orders wo
       LEFT JOIN users u_assign ON wo.assigned_to = u_assign.id
       LEFT JOIN users u_handler ON wo.handled_by = u_handler.id
       LEFT JOIN users u_creator ON wo.created_by = u_creator.id
       WHERE DATE(wo.created_at) = ? OR DATE(wo.handled_at) = ? OR DATE(wo.completed_at) = ?
          OR (wo.status IN ('processing', 'assigned') AND DATE(wo.updated_at) = ?)
       ORDER BY wo.updated_at DESC`, [date, date, date, date]);
    }
    getTasks(date) {
        return this.db.query(`SELECT DISTINCT t.id, COALESCE(t.task_no,'') AS task_no, t.name AS task_name,
              COALESCE(t.status,'') AS status, COALESCE(t.priority,'') AS priority,
              t.assigned_to, t.created_by,
              COALESCE(p.name,'') AS project_name, COALESCE(ph.name,'') AS phase_name,
              COALESCE(t.updated_at,'') AS updated_at
       FROM project_tasks t
       LEFT JOIN projects p ON t.project_id = p.id
       LEFT JOIN project_phases ph ON t.phase_id = ph.id
       WHERE DATE(t.created_at) = ? OR DATE(t.updated_at) = ?
          OR t.id IN (SELECT task_id FROM project_task_logs WHERE DATE(created_at) = ?)
       ORDER BY t.updated_at DESC`, [date, date, date]);
    }
    getTaskLogs(date) {
        return this.db.query(`SELECT l.task_id, COALESCE(l.log_type,'') AS log_type, COALESCE(l.content,'') AS content,
              COALESCE(l.created_at,'') AS created_at, COALESCE(t.task_no,'') AS task_no,
              COALESCE(t.name,'') AS task_name,
              COALESCE(NULLIF(u.display_name,''), u.username,'') AS creator_name
       FROM project_task_logs l
       LEFT JOIN project_tasks t ON l.task_id = t.id
       LEFT JOIN users u ON l.created_by = u.id
       WHERE DATE(l.created_at) = ?
       ORDER BY l.created_at DESC`, [date]);
    }
    getNoteComments(date) {
        return this.db.query(`SELECT nc.id, COALESCE(nc.content,'') AS content, COALESCE(nc.created_at,'') AS created_at,
              COALESCE(nc.created_by,0) AS created_by, COALESCE(nc.parent_id,0) AS parent_id,
              COALESCE(nc.status,'') AS status, COALESCE(n.note_no,'') AS note_no,
              COALESCE(n.title,'') AS note_title, COALESCE(u.username,'') AS username,
              COALESCE(u.display_name,'') AS display_name
       FROM note_comments nc
       LEFT JOIN notes n ON nc.note_id = n.id
       LEFT JOIN users u ON nc.created_by = u.id
       WHERE DATE(nc.created_at) = ?
         AND (n.note_no IS NULL OR n.note_no != 'NTE20260606002')
       ORDER BY nc.created_at ASC`, [date]);
    }
    getUsers() {
        return this.db.query(`SELECT id, username, COALESCE(NULLIF(display_name,''), username) AS display_name
       FROM users WHERE active = 1 ORDER BY username`);
    }
    getReport(date, user) {
        const d = this.validDate(date);
        const workOrders = this.getWorkOrders(d);
        const tasks = this.getTasks(d);
        const taskLogs = this.getTaskLogs(d);
        const noteComments = this.getNoteComments(d);
        let users = this.getUsers();
        if (user.role !== 'admin') {
            users = users.filter((u) => u.id === user.id);
        }
        const reports = [];
        for (const u of users) {
            const uid = u.id;
            const displayName = u.display_name;
            const userWo = workOrders.filter((w) => w.assignedTo === uid || w.handledBy === uid || w.createdBy === uid);
            const userTasks = tasks.filter((t) => t.assignedTo === uid || t.createdBy === uid);
            const userLogs = taskLogs.filter((l) => l.creatorName === displayName || l.creatorName === u.username);
            const userNotes = noteComments.filter((n) => n.createdBy === uid);
            const counts = {
                wo: userWo.length,
                task: userTasks.length,
                log: userLogs.length,
                note: userNotes.length,
            };
            if (counts.wo === 0 && counts.task === 0 && counts.log === 0 && counts.note === 0) {
                continue;
            }
            reports.push({
                id: u.id,
                username: u.username,
                displayName,
                workOrders: userWo,
                tasks: userTasks,
                taskLogs: userLogs,
                noteComments: userNotes,
                counts,
            });
        }
        return { date: d, reports };
    }
};
exports.DailyReportService = DailyReportService;
exports.DailyReportService = DailyReportService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [database_service_1.DatabaseService])
], DailyReportService);
//# sourceMappingURL=daily-report.service.js.map