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
exports.NotesController = void 0;
const common_1 = require("@nestjs/common");
const passport_1 = require("@nestjs/passport");
const notes_service_1 = require("./notes.service");
let NotesController = class NotesController {
    constructor(notesService) {
        this.notesService = notesService;
    }
    list(req) {
        return this.notesService.list(req.user);
    }
    detail(id) {
        return this.notesService.detail(Number(id));
    }
    create(req, body) {
        return this.notesService.create(req.user, body.text || '');
    }
    update(id, body) {
        return this.notesService.update(Number(id), body.title || '', body.content || '');
    }
    patchStatus(id, body) {
        return this.notesService.patchStatus(Number(id), body.status || 'pending');
    }
    patchImportance(id, body) {
        return this.notesService.patchImportance(Number(id), body.importance || 0);
    }
    togglePin(id) {
        return this.notesService.togglePin(Number(id));
    }
    delete(id) {
        return this.notesService.delete(Number(id));
    }
    comments(id) {
        return this.notesService.comments(Number(id));
    }
    addComment(req, id, body) {
        return this.notesService.addComment(req.user, Number(id), body.content || '');
    }
    markComment(commentId, body) {
        return this.notesService.markComment(Number(commentId), !!body.completed);
    }
    deleteComment(commentId) {
        return this.notesService.deleteComment(Number(commentId));
    }
};
exports.NotesController = NotesController;
__decorate([
    (0, common_1.Get)(),
    __param(0, (0, common_1.Request)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object]),
    __metadata("design:returntype", Array)
], NotesController.prototype, "list", null);
__decorate([
    (0, common_1.Get)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Object)
], NotesController.prototype, "detail", null);
__decorate([
    (0, common_1.Post)(),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, Object]),
    __metadata("design:returntype", Object)
], NotesController.prototype, "create", null);
__decorate([
    (0, common_1.Put)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Object)
], NotesController.prototype, "update", null);
__decorate([
    (0, common_1.Patch)(':id/status'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Object)
], NotesController.prototype, "patchStatus", null);
__decorate([
    (0, common_1.Patch)(':id/importance'),
    __param(0, (0, common_1.Param)('id')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Object)
], NotesController.prototype, "patchImportance", null);
__decorate([
    (0, common_1.Patch)(':id/pin'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Object)
], NotesController.prototype, "togglePin", null);
__decorate([
    (0, common_1.Delete)(':id'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Object)
], NotesController.prototype, "delete", null);
__decorate([
    (0, common_1.Get)(':id/comments'),
    __param(0, (0, common_1.Param)('id')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Array)
], NotesController.prototype, "comments", null);
__decorate([
    (0, common_1.Post)(':id/comments'),
    __param(0, (0, common_1.Request)()),
    __param(1, (0, common_1.Param)('id')),
    __param(2, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [Object, String, Object]),
    __metadata("design:returntype", Array)
], NotesController.prototype, "addComment", null);
__decorate([
    (0, common_1.Patch)('comments/:commentId/mark'),
    __param(0, (0, common_1.Param)('commentId')),
    __param(1, (0, common_1.Body)()),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String, Object]),
    __metadata("design:returntype", Object)
], NotesController.prototype, "markComment", null);
__decorate([
    (0, common_1.Delete)('comments/:commentId'),
    __param(0, (0, common_1.Param)('commentId')),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [String]),
    __metadata("design:returntype", Object)
], NotesController.prototype, "deleteComment", null);
exports.NotesController = NotesController = __decorate([
    (0, common_1.Controller)('notes'),
    (0, common_1.UseGuards)((0, passport_1.AuthGuard)('jwt')),
    __metadata("design:paramtypes", [notes_service_1.NotesService])
], NotesController);
//# sourceMappingURL=notes.controller.js.map