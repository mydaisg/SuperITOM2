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
exports.AutomationGateway = void 0;
const websockets_1 = require("@nestjs/websockets");
const socket_io_1 = require("socket.io");
const automation_service_1 = require("./automation.service");
const execute_dto_1 = require("./dto/execute.dto");
let AutomationGateway = class AutomationGateway {
    constructor(automationService) {
        this.automationService = automationService;
    }
    afterInit(server) {
        console.log('WebSocket gateway initialized');
    }
    handleConnection(client) {
        console.log(`Client connected: ${client.id}`);
    }
    handleDisconnect(client) {
        console.log(`Client disconnected: ${client.id}`);
    }
    async handleExecuteScript(client, executeDto) {
        try {
            const executionId = `exec_${Date.now()}_${client.id}`;
            const output$ = this.automationService.executeScript(executeDto);
            output$.subscribe((data) => {
                client.emit('scriptOutput', { executionId, data });
            }, (error) => {
                client.emit('scriptError', { executionId, error: error.message });
            }, () => {
                client.emit('scriptComplete', { executionId });
            });
            return { executionId, status: 'started' };
        }
        catch (error) {
            return { error: error.message };
        }
    }
    handleTerminateScript(client, executionId) {
        const success = this.automationService.terminateScript(executionId);
        return { success };
    }
    handleGetScripts(client) {
        const scripts = this.automationService.getAvailableScripts();
        return { scripts };
    }
};
exports.AutomationGateway = AutomationGateway;
__decorate([
    (0, websockets_1.WebSocketServer)(),
    __metadata("design:type", socket_io_1.Server)
], AutomationGateway.prototype, "server", void 0);
__decorate([
    (0, websockets_1.SubscribeMessage)('executeScript'),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [socket_io_1.Socket, execute_dto_1.ExecuteDto]),
    __metadata("design:returntype", Promise)
], AutomationGateway.prototype, "handleExecuteScript", null);
__decorate([
    (0, websockets_1.SubscribeMessage)('terminateScript'),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [socket_io_1.Socket, String]),
    __metadata("design:returntype", void 0)
], AutomationGateway.prototype, "handleTerminateScript", null);
__decorate([
    (0, websockets_1.SubscribeMessage)('getScripts'),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", [socket_io_1.Socket]),
    __metadata("design:returntype", void 0)
], AutomationGateway.prototype, "handleGetScripts", null);
exports.AutomationGateway = AutomationGateway = __decorate([
    (0, websockets_1.WebSocketGateway)({
        cors: {
            origin: '*',
            methods: ['GET', 'POST'],
        },
    }),
    __metadata("design:paramtypes", [automation_service_1.AutomationService])
], AutomationGateway);
//# sourceMappingURL=automation.gateway.js.map