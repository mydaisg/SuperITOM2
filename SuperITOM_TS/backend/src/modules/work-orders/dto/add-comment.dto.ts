import { IsString, IsNotEmpty } from 'class-validator';

export class AddCommentDto {
  @IsString()
  @IsNotEmpty({ message: '评论内容不能为空' })
  comment: string;
}
