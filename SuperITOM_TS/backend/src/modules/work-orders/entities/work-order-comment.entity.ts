import {
  Entity,
  PrimaryGeneratedColumn,
  Column,
  CreateDateColumn,
  Index,
} from 'typeorm';

@Entity('work_order_comments')
export class WorkOrderCommentEntity {
  @PrimaryGeneratedColumn()
  id: number;

  @Index()
  @Column()
  workOrderId: number;

  @Column({ type: 'text' })
  comment: string;

  @Column()
  creatorId: number;

  @CreateDateColumn()
  createdAt: Date;
}
