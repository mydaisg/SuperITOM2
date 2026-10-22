"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.DailyReportModule = void 0;
const common_1 = require("@nestjs/common");
const daily_report_controller_1 = require("./daily-report.controller");
const daily_report_service_1 = require("./daily-report.service");
let DailyReportModule = class DailyReportModule {
};
exports.DailyReportModule = DailyReportModule;
exports.DailyReportModule = DailyReportModule = __decorate([
    (0, common_1.Module)({
        controllers: [daily_report_controller_1.DailyReportController],
        providers: [daily_report_service_1.DailyReportService],
    })
], DailyReportModule);
//# sourceMappingURL=daily-report.module.js.map