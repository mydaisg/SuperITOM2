import { RequirementsService, Requirement, RequirementProgress } from './requirements.service';
import { AuthUser } from '../auth/auth.service';
export declare class RequirementsController {
    private readonly service;
    constructor(service: RequirementsService);
    list(status?: string, keyword?: string): Requirement[];
    detail(id: string): Requirement;
    create(req: {
        user: AuthUser;
    }, body: {
        title?: string;
        description?: string;
        status?: string;
        priority?: string;
        owner?: string;
        startDate?: string;
        dueDate?: string;
    }): Requirement;
    update(id: string, body: {
        title?: string;
        description?: string;
        status?: string;
        priority?: string;
        owner?: string;
        startDate?: string;
        dueDate?: string;
    }): Requirement;
    delete(id: string): {
        message: string;
    };
    progress(id: string): RequirementProgress[];
    addProgress(id: string, body: {
        dept?: string;
        person?: string;
        status?: string;
        progressDate?: string;
        content?: string;
    }): RequirementProgress[];
    updateProgress(progressId: string, body: {
        dept?: string;
        person?: string;
        status?: string;
        progressDate?: string;
        content?: string;
    }): {
        message: string;
    };
    deleteProgress(progressId: string): {
        message: string;
    };
}
