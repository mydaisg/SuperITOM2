import { IsNumber, IsString, IsBoolean, IsDate } from 'class-validator';

export class UserDto {
  @IsNumber()
  id: number;

  @IsString()
  username: string;

  @IsString()
  role: string;

  @IsString()
  displayName: string;

  @IsBoolean()
  isActive: boolean;

  @IsDate()
  createdAt: Date;

  @IsDate()
  updatedAt: Date;
}