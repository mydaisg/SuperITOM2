import { NotesService, Note, NoteComment } from './notes.service';
import { AuthUser } from '../auth/auth.service';
export declare class NotesController {
    private readonly notesService;
    constructor(notesService: NotesService);
    list(req: {
        user: AuthUser;
    }): Note[];
    detail(id: string): Note;
    create(req: {
        user: AuthUser;
    }, body: {
        text?: string;
    }): Note;
    update(id: string, body: {
        title?: string;
        content?: string;
    }): Note;
    patchStatus(id: string, body: {
        status?: string;
    }): Note;
    patchImportance(id: string, body: {
        importance?: number;
    }): Note;
    togglePin(id: string): Note;
    delete(id: string): {
        message: string;
    };
    comments(id: string): NoteComment[];
    addComment(req: {
        user: AuthUser;
    }, id: string, body: {
        content?: string;
    }): NoteComment[];
    markComment(commentId: string, body: {
        completed?: boolean;
    }): {
        message: string;
    };
    deleteComment(commentId: string): {
        message: string;
    };
}
