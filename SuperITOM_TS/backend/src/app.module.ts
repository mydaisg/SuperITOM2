import { Module } from '@nestjs/common';
import { ConfigModule, ConfigService } from '@nestjs/config';
import { JwtModule } from '@nestjs/jwt';
import { PassportModule } from '@nestjs/passport';
import { TerminusModule } from '@nestjs/terminus';
import { DatabaseModule } from './common/database/database.module';
import { HealthModule } from './common/health/health.module';
import { LoggerModule } from './common/logger/logger.module';
import { AuthModule } from './modules/auth/auth.module';
import { NotesModule } from './modules/notes/notes.module';
import { RequirementsModule } from './modules/requirements/requirements.module';
import { DailyReportModule } from './modules/daily-report/daily-report.module';

@Module({
  imports: [
    // 配置模块
    ConfigModule.forRoot({
      isGlobal: true,
      envFilePath: '.env',
    }),

    // 数据库模块（better-sqlite3 直连现有 GH_ITOM.db，与 R 版共享数据）
    DatabaseModule,

    // JWT 模块
    JwtModule.registerAsync({
      useFactory: (configService: ConfigService) => ({
        secret: configService.get('JWT_SECRET') || 'secret',
        signOptions: {
          expiresIn: configService.get('JWT_EXPIRES_IN') || '24h',
        },
      }),
      inject: [ConfigService],
    }),

    PassportModule,
    TerminusModule,

    // 业务模块
    AuthModule,
    NotesModule,
    RequirementsModule,
    DailyReportModule,

    // 公共模块
    HealthModule,
    LoggerModule,
  ],
  controllers: [],
  providers: [],
})
export class AppModule {}
