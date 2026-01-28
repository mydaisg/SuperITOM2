import { Module } from '@nestjs/common';
import { AutomationService } from './automation.service';
import { AutomationController } from './automation.controller';
import { AutomationGateway } from './automation.gateway';

@Module({
  controllers: [AutomationController],
  providers: [AutomationService, AutomationGateway],
  exports: [AutomationService],
})
export class AutomationModule {}