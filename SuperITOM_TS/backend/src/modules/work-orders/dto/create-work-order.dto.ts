import { IsString, IsNotEmpty, IsOptional } from 'class-validator';

export class CreateWorkOrderDto {
  @IsString()
  @IsNotEmpty({ message: '标题不能为空' })
  title: string;

  @IsString()
  @IsOptional()
  description?: string;

  @IsString()
  @IsOptional()
  priority?: string;

  @IsString()
  @IsOptional()
  category?: string;

  @IsString()
  @IsOptional()
  subcategory?: string;

  @IsString()
  @IsOptional()
  requestUser?: string;
}
