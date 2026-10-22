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
export declare class RequirementsService {
    private readonly db;
    constructor(db: DatabaseService);
    private mapReq;
    private mapProg;
    private generateReqNo;
    list(status?: string, keyword?: string): Requirement[];
    detail(id: number): Requirement;
    create(dto: {
        title?: string;
        description?: string;
        status?: string;
        priority?: string;
        owner?: string;
        startDate?: string;
        dueDate?: string;
    }, userId: number): Requirement;
    update(id: number, dto: {
        title?: string;
        description?: string;
        status?: string;
        priority?: string;
        owner?: string;
        startDate?: string;
        dueDate?: string;
    }): Requirement;
    delete(id: number): {
        message: string;
    };
    progress(id: number): RequirementProgress[];
    addProgress(id: number, dto: {
        dept?: string;
        person?: string;
        status?: string;
        progressDate?: string;
        content?: string;
    }): RequirementProgress[];
    updateProgress(progressId: number, dto: {
        dept?: string;
        person?: string;
        status?: string;
        progressDate?: string;
        content?: string;
    }): {
        message: string;
    };
    deleteProgress(progressId: number): {
        message: string;
    };
}
