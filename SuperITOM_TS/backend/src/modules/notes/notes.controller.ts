import {
  Controller,
  Get,
  Post,
  Put,
  Patch,
  Delete,
  Body,
  Param,
  Request,
  UseGuards,
} from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';
import { NotesService, Note, NoteComment } from './notes.service';
import { AuthUser } from '../auth/auth.service';

@Controller('notes')
@UseGuards(AuthGuard('jwt'))
export class NotesController {
  constructor(private readonly notesService: NotesService) {}

  @Get()
  list(@Request() req: { user: AuthUser }): Note[] {
    return this.notesService.list(req.user);
  }

  @Get(':id')
  detail(@Param('id') id: string): Note {
    return this.notesService.detail(Number(id));
  }

  @Post()
  create(@Request() req: { user: AuthUser }, @Body() body: { text?: string }): Note {
    return this.notesService.create(req.user, body.text || '');
  }

  @Put(':id')
  update(
    @Param('id') id: string,
    @Body() body: { title?: string; content?: string },
  ): Note {
    return this.notesService.update(Number(id), body.title || '', body.content || '');
  }

  @Patch(':id/status')
  patchStatus(@Param('id') id: string, @Body() body: { status?: string }): Note {
    return this.notesService.patchStatus(Number(id), body.status || 'pending');
  }

  @Patch(':id/importance')
  patchImportance(@Param('id') id: string, @Body() body: { importance?: number }): Note {
    return this.notesService.patchImportance(Number(id), body.importance || 0);
  }

  @Patch(':id/pin')
  togglePin(@Param('id') id: string): Note {
    return this.notesService.togglePin(Number(id));
  }

  @Delete(':id')
  delete(@Param('id') id: string): { message: string } {
    return this.notesService.delete(Number(id));
  }

  @Get(':id/comments')
  comments(@Param('id') id: string): NoteComment[] {
    return this.notesService.comments(Number(id));
  }

  @Post(':id/comments')
  addComment(
    @Request() req: { user: AuthUser },
    @Param('id') id: string,
    @Body() body: { content?: string },
  ): NoteComment[] {
    return this.notesService.addComment(req.user, Number(id), body.content || '');
  }

  @Patch('comments/:commentId/mark')
  markComment(
    @Param('commentId') commentId: string,
    @Body() body: { completed?: boolean },
  ): { message: string } {
    return this.notesService.markComment(Number(commentId), !!body.completed);
  }

  @Delete('comments/:commentId')
  deleteComment(@Param('commentId') commentId: string): { message: string } {
    return this.notesService.deleteComment(Number(commentId));
  }
}
