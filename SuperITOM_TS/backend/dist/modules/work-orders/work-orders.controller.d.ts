import { WorkOrdersService, AuthUser } from './work-orders.service';
import { CreateWorkOrderDto } from './dto/create-work-order.dto';
import { AssignWorkOrderDto } from './dto/assign-work-order.dto';
import { CompleteWorkOrderDto } from './dto/complete-work-order.dto';
import { CloseWorkOrderDto } from './dto/close-work-order.dto';
import { AddCommentDto } from './dto/add-comment.dto';
export declare class WorkOrdersController {
    private readonly workOrdersService;
    constructor(workOrdersService: WorkOrdersService);
    list(req: {
        user: AuthUser;
    }, status?: string): Promise<{
        id: number;
        orderNo: string;
        title: string;
        priority: string;
        status: string;
        category: string;
        creatorName: string;
        assigneeName: string;
        createdAt: string;
    }[]>;
    assignableUsers(): Promise<{
        id: number;
        username: string;
        role: string;
        displayName: string;
    }[]>;
    create(req: {
        user: AuthUser;
    }, dto: CreateWorkOrderDto): Promise<{
        id: number;
        orderNo: string;
        title: string;
        description: string;
        priority: string;
        status: string;
        category: string;
        subcategory: string;
        requestUser: string;
        resolution: string;
        closeReason: string;
        creatorId: number;
        assigneeId: number | null;
        handlerId: number | null;
        creatorName: string;
        assigneeName: string;
        handlerName: string;
        createdAt: string;
        assignedAt: string;
        completedAt: string;
        closedAt: string;
        commentCount: number;
    }>;
    detail(id: number): Promise<{
        id: number;
        orderNo: string;
        title: string;
        description: string;
        priority: string;
        status: string;
        category: string;
        subcategory: string;
        requestUser: string;
        resolution: string;
        closeReason: string;
        creatorId: number;
        assigneeId: number | null;
        handlerId: number | null;
        creatorName: string;
        assigneeName: string;
        handlerName: string;
        createdAt: string;
        assignedAt: string;
        completedAt: string;
        closedAt: string;
        commentCount: number;
    }>;
    comments(id: number): Promise<{
        id: number;
        comment: string;
        createdAt: string;
        creatorName: string;
    }[]>;
    addComment(req: {
        user: AuthUser;
    }, id: number, dto: AddCommentDto): Promise<{
        id: number;
        comment: string;
        createdAt: string;
        creatorName: string;
    }[]>;
    assign(req: {
        user: AuthUser;
    }, id: number, dto: AssignWorkOrderDto): Promise<{
        id: number;
        orderNo: string;
        title: string;
        description: string;
        priority: string;
        status: string;
        category: string;
        subcategory: string;
        requestUser: string;
        resolution: string;
        closeReason: string;
        creatorId: number;
        assigneeId: number | null;
        handlerId: number | null;
        creatorName: string;
        assigneeName: string;
        handlerName: string;
        createdAt: string;
        assignedAt: string;
        completedAt: string;
        closedAt: string;
        commentCount: number;
    }>;
    start(req: {
        user: AuthUser;
    }, id: number): Promise<{
        id: number;
        orderNo: string;
        title: string;
        description: string;
        priority: string;
        status: string;
        category: string;
        subcategory: string;
        requestUser: string;
        resolution: string;
        closeReason: string;
        creatorId: number;
        assigneeId: number | null;
        handlerId: number | null;
        creatorName: string;
        assigneeName: string;
        handlerName: string;
        createdAt: string;
        assignedAt: string;
        completedAt: string;
        closedAt: string;
        commentCount: number;
    }>;
    complete(req: {
        user: AuthUser;
    }, id: number, dto: CompleteWorkOrderDto): Promise<{
        id: number;
        orderNo: string;
        title: string;
        description: string;
        priority: string;
        status: string;
        category: string;
        subcategory: string;
        requestUser: string;
        resolution: string;
        closeReason: string;
        creatorId: number;
        assigneeId: number | null;
        handlerId: number | null;
        creatorName: string;
        assigneeName: string;
        handlerName: string;
        createdAt: string;
        assignedAt: string;
        completedAt: string;
        closedAt: string;
        commentCount: number;
    }>;
    close(req: {
        user: AuthUser;
    }, id: number, dto: CloseWorkOrderDto): Promise<{
        id: number;
        orderNo: string;
        title: string;
        description: string;
        priority: string;
        status: string;
        category: string;
        subcategory: string;
        requestUser: string;
        resolution: string;
        closeReason: string;
        creatorId: number;
        assigneeId: number | null;
        handlerId: number | null;
        creatorName: string;
        assigneeName: string;
        handlerName: string;
        createdAt: string;
        assignedAt: string;
        completedAt: string;
        closedAt: string;
        commentCount: number;
    }>;
}
