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
exports.WorkOrderEntity = exports.WorkOrderStatus = void 0;
const typeorm_1 = require("typeorm");
var WorkOrderStatus;
(function (WorkOrderStatus) {
    WorkOrderStatus["PENDING"] = "pending";
    WorkOrderStatus["ASSIGNED"] = "assigned";
    WorkOrderStatus["PROCESSING"] = "processing";
    WorkOrderStatus["COMPLETED"] = "completed";
    WorkOrderStatus["CLOSED"] = "closed";
})(WorkOrderStatus || (exports.WorkOrderStatus = WorkOrderStatus = {}));
let WorkOrderEntity = class WorkOrderEntity {
};
exports.WorkOrderEntity = WorkOrderEntity;
__decorate([
    (0, typeorm_1.PrimaryGeneratedColumn)(),
    __metadata("design:type", Number)
], WorkOrderEntity.prototype, "id", void 0);
__decorate([
    (0, typeorm_1.Index)({ unique: true }),
    (0, typeorm_1.Column)(),
    __metadata("design:type", String)
], WorkOrderEntity.prototype, "orderNo", void 0);
__decorate([
    (0, typeorm_1.Column)(),
    __metadata("design:type", String)
], WorkOrderEntity.prototype, "title", void 0);
__decorate([
    (0, typeorm_1.Column)({ type: 'text', default: '' }),
    __metadata("design:type", String)
], WorkOrderEntity.prototype, "description", void 0);
__decorate([
    (0, typeorm_1.Column)({ default: WorkOrderStatus.PENDING }),
    __metadata("design:type", String)
], WorkOrderEntity.prototype, "status", void 0);
__decorate([
    (0, typeorm_1.Column)({ default: '中' }),
    __metadata("design:type", String)
], WorkOrderEntity.prototype, "priority", void 0);
__decorate([
    (0, typeorm_1.Column)({ default: '一般' }),
    __metadata("design:type", String)
], WorkOrderEntity.prototype, "category", void 0);
__decorate([
    (0, typeorm_1.Column)({ default: '' }),
    __metadata("design:type", String)
], WorkOrderEntity.prototype, "subcategory", void 0);
__decorate([
    (0, typeorm_1.Column)({ default: '' }),
    __metadata("design:type", String)
], WorkOrderEntity.prototype, "requestUser", void 0);
__decorate([
    (0, typeorm_1.Column)({ type: 'text', default: '' }),
    __metadata("design:type", String)
], WorkOrderEntity.prototype, "resolution", void 0);
__decorate([
    (0, typeorm_1.Column)({ type: 'text', default: '' }),
    __metadata("design:type", String)
], WorkOrderEntity.prototype, "closeReason", void 0);
__decorate([
    (0, typeorm_1.Column)(),
    __metadata("design:type", Number)
], WorkOrderEntity.prototype, "creatorId", void 0);
__decorate([
    (0, typeorm_1.Column)({ type: 'int', nullable: true }),
    __metadata("design:type", Object)
], WorkOrderEntity.prototype, "assigneeId", void 0);
__decorate([
    (0, typeorm_1.Column)({ type: 'int', nullable: true }),
    __metadata("design:type", Object)
], WorkOrderEntity.prototype, "handlerId", void 0);
__decorate([
    (0, typeorm_1.CreateDateColumn)(),
    __metadata("design:type", Date)
], WorkOrderEntity.prototype, "createdAt", void 0);
__decorate([
    (0, typeorm_1.UpdateDateColumn)(),
    __metadata("design:type", Date)
], WorkOrderEntity.prototype, "updatedAt", void 0);
__decorate([
    (0, typeorm_1.Column)({ type: 'timestamp', nullable: true }),
    __metadata("design:type", Object)
], WorkOrderEntity.prototype, "assignedAt", void 0);
__decorate([
    (0, typeorm_1.Column)({ type: 'timestamp', nullable: true }),
    __metadata("design:type", Object)
], WorkOrderEntity.prototype, "completedAt", void 0);
__decorate([
    (0, typeorm_1.Column)({ type: 'timestamp', nullable: true }),
    __metadata("design:type", Object)
], WorkOrderEntity.prototype, "closedAt", void 0);
exports.WorkOrderEntity = WorkOrderEntity = __decorate([
    (0, typeorm_1.Entity)('work_orders')
], WorkOrderEntity);
//# sourceMappingURL=work-order.entity.js.map