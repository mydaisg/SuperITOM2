import { Module } from '@nestjs/common';
import { ConfigModule, ConfigService } from '@nestjs/config';
import { TypeOrmModule } from '@nestjs/typeorm';
import { JwtModule } from '@nestjs/jwt';
import { PassportModule } from '@nestjs/passport';
import { ScheduleModule } from '@nestjs/schedule';
import { TerminusModule } from '@nestjs/terminus';
import { AuthModule } from './modules/auth/auth.module';
import { ConfigModule as ItomConfigModule } from './modules/config/config.module';
import { DataModule } from './modules/data/data.module';
import { AutomationModule } from './modules/automation/automation.module';
import { GitHubModule } from './modules/github/github.module';
import { HealthModule } from './common/health/health.module';
import { LoggerModule } from './common/logger/logger.module';

@Module({
  imports: [
    // 配置模块
    ConfigModule.forRoot({
      isGlobal: true,
      envFilePath: '.env',
    }),
    
    // 数据库模块
    TypeOrmModule.forRootAsync({
      useFactory: (configService: ConfigService) => ({
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
      inject: [ConfigService],
    }),
    
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
    
    // 其他模块
    PassportModule,
    ScheduleModule.forRoot(),
    TerminusModule,
    
    // 业务模块
    AuthModule,
    ItomConfigModule,
    DataModule,
    AutomationModule,
    GitHubModule,
    
    // 公共模块
    HealthModule,
    LoggerModule,
  ],
  controllers: [],
  providers: [],
})
export class AppModule {}