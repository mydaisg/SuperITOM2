import { DatabaseService } from '../../common/database/database.service';
import { AuthUser } from '../auth/auth.service';
export interface Note {
    id: number;
    noteNo: string;
    title: string;
    content: string;
    status: string;
    importance: number;
    pinned: number;
    reminderAt: string;
    dueAt: string;
    createdBy: number;
    createdAt: string;
    updatedAt: string;
    creatorName: string;
    commentCount: number;
}
export interface NoteComment {
    id: number;
    noteId: number;
    content: string;
    createdBy: number;
    createdAt: string;
    status: string;
    completedAt: string;
    parentId: number;
    creatorName: string;
}
export declare class NotesService {
    private readonly db;
    constructor(db: DatabaseService);
    private mapNote;
    private mapComment;
    private generateNoteNo;
    list(user: AuthUser): Note[];
    detail(id: number): Note;
    create(user: AuthUser, text: string): Note;
    update(id: number, title: string, content: string): Note;
    patchStatus(id: number, status: string): Note;
    patchImportance(id: number, importance: number): Note;
    togglePin(id: number): Note;
    delete(id: number): {
        message: string;
    };
    comments(id: number): NoteComment[];
    addComment(user: AuthUser, id: number, content: string): NoteComment[];
    markComment(commentId: number, completed: boolean): {
        message: string;
    };
    deleteComment(commentId: number): {
        message: string;
    };
}
