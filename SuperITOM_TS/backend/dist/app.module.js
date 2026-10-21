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
const typeorm_1 = require("@nestjs/typeorm");
const jwt_1 = require("@nestjs/jwt");
const passport_1 = require("@nestjs/passport");
const schedule_1 = require("@nestjs/schedule");
const terminus_1 = require("@nestjs/terminus");
const auth_module_1 = require("./modules/auth/auth.module");
const config_module_1 = require("./modules/config/config.module");
const data_module_1 = require("./modules/data/data.module");
const automation_module_1 = require("./modules/automation/automation.module");
const github_module_1 = require("./modules/github/github.module");
const work_orders_module_1 = require("./modules/work-orders/work-orders.module");
const health_module_1 = require("./common/health/health.module");
const logger_module_1 = require("./common/logger/logger.module");
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
            typeorm_1.TypeOrmModule.forRootAsync({
                useFactory: (configService) => ({
                    type: 'postgres',
                    host: configService.get('DATABASE_HOST') || 'localhost',
                    port: parseInt(configService.get('DATABASE_PORT') || '5432'),
                    username: configService.get('DATABASE_USERNAME') || 'admin',
                    password: configService.get('DATABASE_PASSWORD') || 'password',
                    database: configService.get('DATABASE_NAME') || 'superitom',
                    entities: [__dirname + '/**/*.entity{.ts,.js}'],
                    synchronize: true,
                    logging: true,
                }),
                inject: [config_1.ConfigService],
            }),
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
            schedule_1.ScheduleModule.forRoot(),
            terminus_1.TerminusModule,
            auth_module_1.AuthModule,
            config_module_1.ConfigModule,
            data_module_1.DataModule,
            automation_module_1.AutomationModule,
            github_module_1.GitHubModule,
            work_orders_module_1.WorkOrdersModule,
            health_module_1.HealthModule,
            logger_module_1.LoggerModule,
        ],
        controllers: [],
        providers: [],
    })
], AppModule);
//# sourceMappingURL=app.module.js.map