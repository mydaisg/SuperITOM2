import { IsString, IsOptional } from 'class-validator';

export class CompleteWorkOrderDto {
  @IsString()
  @IsOptional()
  resolution?: string;
}
