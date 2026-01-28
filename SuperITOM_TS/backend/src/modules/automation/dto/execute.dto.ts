import { IsString, IsOptional, IsArray } from 'class-validator';

export class ExecuteDto {
  @IsString()
  script: string;

  @IsArray()
  @IsOptional()
  args?: string[];
}