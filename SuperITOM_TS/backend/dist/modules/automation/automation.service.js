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
exports.AutomationService = void 0;
const common_1 = require("@nestjs/common");
const rxjs_1 = require("rxjs");
const pty = require("node-pty");
const path = require("path");
const fs = require("fs");
let AutomationService = class AutomationService {
    constructor() {
        this.scriptExecutions = new Map();
    }
    getAvailableScripts() {
        const scriptsDir = path.join(__dirname, '..', '..', '..', '..', 'STD');
        if (!fs.existsSync(scriptsDir)) {
            return [];
        }
        const files = fs.readdirSync(scriptsDir);
        const scripts = files.filter(file => file.endsWith('.ps1'));
        return scripts.map(script => ({
            name: script,
            path: path.join(scriptsDir, script),
        }));
    }
    executeScript(executeDto) {
        const { script, args = [] } = executeDto;
        const scriptsDir = path.join(__dirname, '..', '..', '..', '..', 'STD');
        const scriptPath = path.join(scriptsDir, script);
        if (!fs.existsSync(scriptPath)) {
            throw new common_1.NotFoundException(`脚本 ${script} 不存在`);
        }
        const executionId = this.generateExecutionId();
        const outputSubject = new rxjs_1.Subject();
        try {
            const ptyProcess = pty.spawn('powershell.exe', ['-File', scriptPath, ...args], {
                name: 'xterm-color',
                cols: 80,
                rows: 30,
                cwd: process.cwd(),
                env: process.env,
            });
            this.scriptExecutions.set(executionId, { pty: ptyProcess, output: outputSubject });
            ptyProcess.onData((data) => {
                outputSubject.next(data);
            });
            ptyProcess.onExit(() => {
                outputSubject.complete();
                this.scriptExecutions.delete(executionId);
            });
            return outputSubject.asObservable();
        }
        catch (error) {
            outputSubject.error(error);
            return outputSubject.asObservable();
        }
    }
    terminateScript(executionId) {
        const execution = this.scriptExecutions.get(executionId);
        if (execution) {
            execution.pty.kill();
            this.scriptExecutions.delete(executionId);
            return true;
        }
        return false;
    }
    getExecutionStatus(executionId) {
        return this.scriptExecutions.has(executionId);
    }
    generateExecutionId() {
        return `exec_${Date.now()}_${Math.floor(Math.random() * 10000)}`;
    }
};
exports.AutomationService = AutomationService;
exports.AutomationService = AutomationService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [])
], AutomationService);
//# sourceMappingURL=automation.service.js.map