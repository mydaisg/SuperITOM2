import { IsString, IsOptional, IsNumber } from 'class-validator';

export class ExecuteResultDto {
  @IsString()
  output: string;

  @IsNumber()
  @IsOptional()
  exitCode?: number;

  @IsOptional()
  error?: string;
}