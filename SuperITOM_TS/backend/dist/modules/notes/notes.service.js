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
exports.NotesService = void 0;
const common_1 = require("@nestjs/common");
const database_service_1 = require("../../common/database/database.service");
const NOTE_SELECT = `
  SELECT n.id, COALESCE(n.note_no,'') AS note_no, n.title, COALESCE(n.content,'') AS content,
         COALESCE(n.status,'pending') AS status, COALESCE(n.importance,0) AS importance,
         COALESCE(n.pinned,0) AS pinned, COALESCE(n.reminder_at,'') AS reminder_at,
         COALESCE(n.due_at,'') AS due_at, COALESCE(n.created_by,0) AS created_by,
         COALESCE(n.created_at,'') AS created_at, COALESCE(n.updated_at,'') AS updated_at,
         COALESCE(NULLIF(u.display_name,''), u.username, '') AS creator_name,
         (SELECT COUNT(*) FROM note_comments c WHERE c.note_id = n.id) AS comment_count
  FROM notes n LEFT JOIN users u ON n.created_by = u.id`;
let NotesService = class NotesService {
    constructor(db) {
        this.db = db;
    }
    mapNote(r) {
        return {
            id: r.id,
            noteNo: r.note_no,
            title: r.title,
            content: r.content,
            status: r.status,
            importance: r.importance,
            pinned: r.pinned,
            reminderAt: r.reminder_at,
            dueAt: r.due_at,
            createdBy: r.created_by,
            createdAt: r.created_at,
            updatedAt: r.updated_at,
            creatorName: r.creator_name,
            commentCount: r.comment_count,
        };
    }
    mapComment(r) {
        return {
            id: r.id,
            noteId: r.note_id,
            content: r.content,
            createdBy: r.created_by,
            createdAt: r.created_at,
            status: r.status || '',
            completedAt: r.completed_at || '',
            parentId: r.parent_id,
            creatorName: r.creator_name,
        };
    }
    generateNoteNo() {
        const today = this.db.get("SELECT strftime('%Y%m%d','now','localtime') AS d").d;
        const prefix = `NTE${today}`;
        const row = this.db.get('SELECT note_no FROM notes WHERE note_no LIKE ? ORDER BY note_no DESC LIMIT 1', [`${prefix}%`]);
        let seq = 1;
        if (row && row.note_no && row.note_no.length > prefix.length) {
            const n = parseInt(row.note_no.slice(prefix.length), 10);
            if (!isNaN(n))
                seq = n + 1;
        }
        return `${prefix}${String(seq).padStart(3, '0')}`;
    }
    list(user) {
        if (user.role === 'admin') {
            const rows = this.db.query(`${NOTE_SELECT} ORDER BY n.pinned DESC, n.updated_at DESC`);
            return rows.map((r) => this.mapNote(r));
        }
        const rows = this.db.query(`${NOTE_SELECT} WHERE n.created_by = ? ORDER BY n.pinned DESC, n.updated_at DESC`, [user.id]);
        return rows.map((r) => this.mapNote(r));
    }
    detail(id) {
        const row = this.db.get(`${NOTE_SELECT} WHERE n.id = ?`, [id]);
        if (!row) {
            throw new common_1.NotFoundException('记事不存在');
        }
        return this.mapNote(row);
    }
    create(user, text) {
        const trimmed = (text || '').trim();
        if (!trimmed) {
            throw new common_1.BadRequestException('内容不能为空');
        }
        const firstLine = trimmed.split('\n')[0].trim() || '未命名记事';
        const noteNo = this.generateNoteNo();
        const info = this.db.run("INSERT INTO notes (note_no, title, content, status, importance, pinned, created_by, created_at, updated_at) VALUES (?, ?, ?, 'pending', 0, 0, ?, datetime('now','localtime'), datetime('now','localtime'))", [noteNo, firstLine, trimmed, user.id]);
        return this.detail(Number(info.lastInsertRowid));
    }
    update(id, title, content) {
        this.detail(id);
        this.db.run("UPDATE notes SET title = ?, content = ?, updated_at = datetime('now','localtime') WHERE id = ?", [title, content, id]);
        return this.detail(id);
    }
    patchStatus(id, status) {
        this.detail(id);
        this.db.run("UPDATE notes SET status = ?, updated_at = datetime('now','localtime') WHERE id = ?", [status, id]);
        return this.detail(id);
    }
    patchImportance(id, importance) {
        this.detail(id);
        const next = Math.max(0, Math.min(5, Number(importance) || 0));
        this.db.run("UPDATE notes SET importance = ?, updated_at = datetime('now','localtime') WHERE id = ?", [next, id]);
        return this.detail(id);
    }
    togglePin(id) {
        const row = this.db.get('SELECT COALESCE(pinned,0) AS pinned FROM notes WHERE id = ?', [id]);
        if (!row) {
            throw new common_1.NotFoundException('记事不存在');
        }
        if (row.pinned === 0) {
            const count = this.db.get('SELECT COUNT(*) AS n FROM notes WHERE pinned = 1').n;
            if (count >= 5) {
                throw new common_1.BadRequestException('最多置顶5条');
            }
            this.db.run("UPDATE notes SET pinned = 1, updated_at = datetime('now','localtime') WHERE id = ?", [id]);
        }
        else {
            this.db.run('UPDATE notes SET pinned = 0 WHERE id = ?', [id]);
        }
        return this.detail(id);
    }
    delete(id) {
        this.detail(id);
        this.db.run('DELETE FROM note_comments WHERE note_id = ?', [id]);
        this.db.run('DELETE FROM note_dispatches WHERE note_id = ?', [id]);
        this.db.run('DELETE FROM notes WHERE id = ?', [id]);
        return { message: '已删除' };
    }
    comments(id) {
        this.detail(id);
        const rows = this.db.query(`SELECT c.id, c.note_id, COALESCE(c.content,'') AS content, COALESCE(c.created_by,0) AS created_by,
              COALESCE(c.created_at,'') AS created_at, COALESCE(c.status,'') AS status,
              COALESCE(c.completed_at,'') AS completed_at, COALESCE(c.parent_id,0) AS parent_id,
              COALESCE(NULLIF(u.display_name,''), u.username,'') AS creator_name
       FROM note_comments c LEFT JOIN users u ON c.created_by = u.id
       WHERE c.note_id = ? ORDER BY c.created_at ASC, c.id ASC`, [id]);
        return rows.map((r) => this.mapComment(r));
    }
    addComment(user, id, content) {
        this.detail(id);
        const c = (content || '').trim();
        if (!c) {
            throw new common_1.BadRequestException('评论不能为空');
        }
        this.db.run("INSERT INTO note_comments (note_id, content, created_by, created_at) VALUES (?, ?, ?, datetime('now','localtime'))", [id, c, user.id]);
        this.db.run("UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = ?", [id]);
        return this.comments(id);
    }
    markComment(commentId, completed) {
        if (completed) {
            this.db.run("UPDATE note_comments SET status = 'completed', completed_at = datetime('now','localtime') WHERE id = ?", [commentId]);
        }
        else {
            this.db.run('UPDATE note_comments SET status = NULL, completed_at = NULL WHERE id = ?', [commentId]);
        }
        this.db.run("UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = (SELECT note_id FROM note_comments WHERE id = ?)", [commentId]);
        return { message: '已更新' };
    }
    deleteComment(commentId) {
        this.db.run("UPDATE notes SET updated_at = datetime('now','localtime') WHERE id = (SELECT note_id FROM note_comments WHERE id = ?)", [commentId]);
        this.db.run('DELETE FROM note_comments WHERE id = ?', [commentId]);
        return { message: '已删除' };
    }
};
exports.NotesService = NotesService;
exports.NotesService = NotesService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [database_service_1.DatabaseService])
], NotesService);
//# sourceMappingURL=notes.service.js.map