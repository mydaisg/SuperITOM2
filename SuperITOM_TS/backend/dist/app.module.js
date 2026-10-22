"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.AppModule = void 0;
const common_1 = require("@nestjs/common");
const config_1 = require("@nestjs/config");
const jwt_1 = require("@nestjs/jwt");
const passport_1 = require("@nestjs/passport");
const terminus_1 = require("@nestjs/terminus");
const database_module_1 = require("./common/database/database.module");
const health_module_1 = require("./common/health/health.module");
const logger_module_1 = require("./common/logger/logger.module");
const auth_module_1 = require("./modules/auth/auth.module");
const notes_module_1 = require("./modules/notes/notes.module");
const requirements_module_1 = require("./modules/requirements/requirements.module");
const daily_report_module_1 = require("./modules/daily-report/daily-report.module");
let AppModule = class AppModule {
};
exports.AppModule = AppModule;
exports.AppModule = AppModule = __decorate([
    (0, common_1.Module)({
        imports: [
            config_1.ConfigModule.forRoot({
                isGlobal: true,
                envFilePath: '.env',
            }),
            database_module_1.DatabaseModule,
            jwt_1.JwtModule.registerAsync({
                useFactory: (configService) => ({
                    secret: configService.get('JWT_SECRET') || 'secret',
                    signOptions: {
                        expiresIn: configService.get('JWT_EXPIRES_IN') || '24h',
                    },
                }),
                inject: [config_1.ConfigService],
            }),
            passport_1.PassportModule,
            terminus_1.TerminusModule,
            auth_module_1.AuthModule,
            notes_module_1.NotesModule,
            requirements_module_1.RequirementsModule,
            daily_report_module_1.DailyReportModule,
            health_module_1.HealthModule,
            logger_module_1.LoggerModule,
        ],
        controllers: [],
        providers: [],
    })
], AppModule);
//# sourceMappingURL=app.module.js.map