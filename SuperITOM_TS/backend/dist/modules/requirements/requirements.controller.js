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
exports.RequirementsController = void 0;
const common_1 = require("@nestjs/common");
const passport_1 = require("@nestjs/passport");
const requirements_service_1 = require("./requirements.service");
let RequirementsController = class RequirementsController {
    constructor(service) {
        this.service = service;
    }
    list(status, keyword) {
        return this.service.list(status, keyword);
    }
    detail(id) {
        return this.service.detail(Number(id));
    }
    create(req, body) {
        return this.service.create(body, req.user.id);
    }
    update(id, body) {
        return this.service.update(Number(id), body);
    }
    delete(id) {
        return this.service.delete(Number(id));
    }
    progress(id) {
        return this.service.progress(Number(id));
    }
    addProgress(id, body) {
        return this.service.addProgress(Number(id), body);
    }
    updateProgress(progressId, body) {
        return this.service.updateProgress(Number(progressId), body);
    }
    deleteProgress(progressId) {
        return this.service.deleteProgress(Number(progressId));
    }
};
exports.RequirementsController = RequirementsController;
__decorate([
    (0, common_1.Get)(),
    __param(0, (0, common_1.Query)('status')),
    __param(1, (0, common_1.Query)('keyword')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, String]),
    __metadata("design:returntype", Array)
], RequirementsController.prototype, "list", null);
__decorate([
    (0, common_1.Get)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Object)
], RequirementsController.prototype, "detail", null);
__decorate([
    (0, common_1.Post)(),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, Object]),
    __metadata("design:returntype", Object)
], RequirementsController.prototype, "create", null);
__decorate([
    (0, common_1.Put)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Object)
], RequirementsController.prototype, "update", null);
__decorate([
    (0, common_1.Delete)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Object)
], RequirementsController.prototype, "delete", null);
__decorate([
    (0, common_1.Get)(':id/progress'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Array)
], RequirementsController.prototype, "progress", null);
__decorate([
    (0, common_1.Post)(':id/progress'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Array)
], RequirementsController.prototype, "addProgress", null);
__decorate([
    (0, common_1.Put)('progress/:progressId'),
    __param(0, (0, common_1.Param)('progressId')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Object)
], RequirementsController.prototype, "updateProgress", null);
__decorate([
    (0, common_1.Delete)('progress/:progressId'),
    __param(0, (0, common_1.Param)('progressId')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Object)
], RequirementsController.prototype, "deleteProgress", null);
exports.RequirementsController = RequirementsController = __decorate([
    (0, common_1.Controller)('requirements'),
    (0, common_1.UseGuards)((0, passport_1.AuthGuard)('jwt')),
    __metadata("design:paramtypes", [requirements_service_1.RequirementsService])
], RequirementsController);
//# sourceMappingURL=requirements.controller.js.map