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
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.WorkOrdersController = void 0;
const common_1 = require("@nestjs/common");
const passport_1 = require("@nestjs/passport");
const work_orders_service_1 = require("./work-orders.service");
const create_work_order_dto_1 = require("./dto/create-work-order.dto");
const assign_work_order_dto_1 = require("./dto/assign-work-order.dto");
const complete_work_order_dto_1 = require("./dto/complete-work-order.dto");
const close_work_order_dto_1 = require("./dto/close-work-order.dto");
const add_comment_dto_1 = require("./dto/add-comment.dto");
let WorkOrdersController = class WorkOrdersController {
    constructor(workOrdersService) {
        this.workOrdersService = workOrdersService;
    }
    list(req, status) {
        return this.workOrdersService.list(req.user, status);
    }
    assignableUsers() {
        return this.workOrdersService.assignableUsers();
    }
    create(req, dto) {
        return this.workOrdersService.create(req.user, dto);
    }
    detail(id) {
        return this.workOrdersService.detail(id);
    }
    comments(id) {
        return this.workOrdersService.comments(id);
    }
    addComment(req, id, dto) {
        return this.workOrdersService.addComment(req.user, id, dto);
    }
    assign(req, id, dto) {
        return this.workOrdersService.assign(req.user, id, dto);
    }
    start(req, id) {
        return this.workOrdersService.start(req.user, id);
    }
    complete(req, id, dto) {
        return this.workOrdersService.complete(req.user, id, dto);
    }
    close(req, id, dto) {
        return this.workOrdersService.close(req.user, id, dto);
    }
};
exports.WorkOrdersController = WorkOrdersController;
__decorate([
    (0, common_1.Get)(),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Query)('status')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, String]),
    __metadata("design:returntype", void 0)
], WorkOrdersController.prototype, "list", null);
__decorate([
    (0, common_1.Get)('assignable-users'),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", []),
    __metadata("design:returntype", void 0)
], WorkOrdersController.prototype, "assignableUsers", null);
__decorate([
    (0, common_1.Post)(),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, create_work_order_dto_1.CreateWorkOrderDto]),
    __metadata("design:returntype", void 0)
], WorkOrdersController.prototype, "create", null);
__decorate([
    (0, common_1.Get)(':id'),
    __param(0, (0, common_1.Param)('id', common_1.ParseIntPipe)),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Number]),
    __metadata("design:returntype", void 0)
], WorkOrdersController.prototype, "detail", null);
__decorate([
    (0, common_1.Get)(':id/comments'),
    __param(0, (0, common_1.Param)('id', common_1.ParseIntPipe)),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Number]),
    __metadata("design:returntype", void 0)
], WorkOrdersController.prototype, "comments", null);
__decorate([
    (0, common_1.Post)(':id/comments'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Param)('id', common_1.ParseIntPipe)),
    __param(2, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, Number, add_comment_dto_1.AddCommentDto]),
    __metadata("design:returntype", void 0)
], WorkOrdersController.prototype, "addComment", null);
__decorate([
    (0, common_1.Post)(':id/assign'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Param)('id', common_1.ParseIntPipe)),
    __param(2, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, Number, assign_work_order_dto_1.AssignWorkOrderDto]),
    __metadata("design:returntype", void 0)
], WorkOrdersController.prototype, "assign", null);
__decorate([
    (0, common_1.Post)(':id/start'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Param)('id', common_1.ParseIntPipe)),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, Number]),
    __metadata("design:returntype", void 0)
], WorkOrdersController.prototype, "start", null);
__decorate([
    (0, common_1.Post)(':id/complete'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Param)('id', common_1.ParseIntPipe)),
    __param(2, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, Number, complete_work_order_dto_1.CompleteWorkOrderDto]),
    __metadata("design:returntype", void 0)
], WorkOrdersController.prototype, "complete", null);
__decorate([
    (0, common_1.Post)(':id/close'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Param)('id', common_1.ParseIntPipe)),
    __param(2, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, Number, close_work_order_dto_1.CloseWorkOrderDto]),
    __metadata("design:returntype", void 0)
], WorkOrdersController.prototype, "close", null);
exports.WorkOrdersController = WorkOrdersController = __decorate([
    (0, common_1.Controller)('work-orders'),
    (0, common_1.UseGuards)((0, passport_1.AuthGuard)('jwt')),
    __metadata("design:paramtypes", [work_orders_service_1.WorkOrdersService])
], WorkOrdersController);
//# sourceMappingURL=work-orders.controller.js.map