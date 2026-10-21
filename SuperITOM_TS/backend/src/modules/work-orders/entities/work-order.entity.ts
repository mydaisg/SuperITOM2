import {
  Entity,
  PrimaryGeneratedColumn,
  Column,
  CreateDateColumn,
  UpdateDateColumn,
  Index,
} from 'typeorm';

export enum WorkOrderStatus {
  PENDING = 'pending',
  ASSIGNED = 'assigned',
  PROCESSING = 'processing',
  COMPLETED = 'completed',
  CLOSED = 'closed',
}

@Entity('work_orders')
export class WorkOrderEntity {
  @PrimaryGeneratedColumn()
  id: number;

  @Index({ unique: true })
  @Column()
  orderNo: string;

  @Column()
  title: string;

  @Column({ type: 'text', default: '' })
  description: string;

  @Column({ default: WorkOrderStatus.PENDING })
  status: string;

  @Column({ default: '中' })
  priority: string;

  @Column({ default: '一般' })
  category: string;

  @Column({ default: '' })
  subcategory: string;

  @Column({ default: '' })
  requestUser: string;

  @Column({ type: 'text', default: '' })
  resolution: string;

  @Column({ type: 'text', default: '' })
  closeReason: string;

  @Column()
  creatorId: number;

  @Column({ type: 'int', nullable: true })
  assigneeId: number | null;

  @Column({ type: 'int', nullable: true })
  handlerId: number | null;

  @CreateDateColumn()
  createdAt: Date;

  @UpdateDateColumn()
  updatedAt: Date;

  @Column({ type: 'timestamp', nullable: true })
  assignedAt: Date | null;

  @Column({ type: 'timestamp', nullable: true })
  completedAt: Date | null;

  @Column({ type: 'timestamp', nullable: true })
  closedAt: Date | null;
}
