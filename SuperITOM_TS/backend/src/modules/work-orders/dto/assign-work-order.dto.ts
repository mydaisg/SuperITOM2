import { IsInt, IsPositive } from 'class-validator';

export class AssignWorkOrderDto {
  @IsInt()
  @IsPositive()
  assigneeId: number;
}
