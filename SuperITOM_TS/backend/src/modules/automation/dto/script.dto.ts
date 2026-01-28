import { IsString } from 'class-validator';

export class ScriptDto {
  @IsString()
  name: string;

  @IsString()
  path: string;
}