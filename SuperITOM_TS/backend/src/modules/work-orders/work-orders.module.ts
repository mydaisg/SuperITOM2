import { Module } from '@nestjs/common';
import { TypeOrmModule } from '@nestjs/typeorm';
import { WorkOrdersService } from './work-orders.service';
import { WorkOrdersController } from './work-orders.controller';
import { WorkOrderEntity } from './entities/work-order.entity';
import { WorkOrderCommentEntity } from './entities/work-order-comment.entity';
import { UserEntity } from '../auth/entities/user.entity';

@Module({
  imports: [
    TypeOrmModule.forFeature([WorkOrderEntity, WorkOrderCommentEntity, UserEntity]),
  ],
  controllers: [WorkOrdersController],
  providers: [WorkOrdersService],
  exports: [WorkOrdersService],
})
export class WorkOrdersModule {}
