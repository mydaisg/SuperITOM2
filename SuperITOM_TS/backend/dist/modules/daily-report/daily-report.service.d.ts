import { DatabaseService } from '../../common/database/database.service';
import { AuthUser } from '../auth/auth.service';
export interface WorkOrderItem {
    id: number;
    orderNo: string;
    title: string;
    priority: string;
    status: string;
    category: string;
    assignedTo: number;
    handledBy: number;
    createdBy: number;
    createdAt: string;
    handledAt: string;
    completedAt: string;
    assigneeName: string;
    handlerName: string;
    creatorName: string;
}
export interface TaskItem {
    id: number;
    taskNo: string;
    taskName: string;
    status: string;
    priority: string;
    assignedTo: number;
    createdBy: number;
    projectName: string;
    phaseName: string;
    updatedAt: string;
}
export interface TaskLogItem {
    taskId: number;
    logType: string;
    content: string;
    createdAt: string;
    taskNo: string;
    taskName: string;
    creatorName: string;
}
export interface NoteCommentItem {
    id: number;
    content: string;
    createdAt: string;
    createdBy: number;
    parentId: number;
    status: string;
    noteNo: string;
    noteTitle: string;
    username: string;
    displayName: string;
}
export interface UserReport {
    id: number;
    username: string;
    displayName: string;
    workOrders: WorkOrderItem[];
    tasks: TaskItem[];
    taskLogs: TaskLogItem[];
    noteComments: NoteCommentItem[];
    counts: {
        wo: number;
        task: number;
        log: number;
        note: number;
    };
}
export interface DailyReport {
    date: string;
    reports: UserReport[];
}
export declare class DailyReportService {
    private readonly db;
    constructor(db: DatabaseService);
    private validDate;
    private getWorkOrders;
    private getTasks;
    private getTaskLogs;
    private getNoteComments;
    private getUsers;
    getReport(date: string, user: AuthUser): DailyReport;
}
