export declare enum WorkOrderStatus {
    PENDING = "pending",
    ASSIGNED = "assigned",
    PROCESSING = "processing",
    COMPLETED = "completed",
    CLOSED = "closed"
}
export declare class WorkOrderEntity {
    id: number;
    orderNo: string;
    title: string;
    description: string;
    status: string;
    priority: string;
    category: string;
    subcategory: string;
    requestUser: string;
    resolution: string;
    closeReason: string;
    creatorId: number;
    assigneeId: number | null;
    handlerId: number | null;
    createdAt: Date;
    updatedAt: Date;
    assignedAt: Date | null;
    completedAt: Date | null;
    closedAt: Date | null;
}
