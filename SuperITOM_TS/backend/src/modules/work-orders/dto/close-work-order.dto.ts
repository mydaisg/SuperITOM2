import { IsString, IsOptional } from 'class-validator';

export class CloseWorkOrderDto {
  @IsString()
  @IsOptional()
  reason?: string;
}
