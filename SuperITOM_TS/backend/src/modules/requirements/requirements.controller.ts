import {
  Controller,
  Get,
  Post,
  Put,
  Delete,
  Body,
  Param,
  Query,
  Request,
  UseGuards,
} from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';
import {
  RequirementsService,
  Requirement,
  RequirementProgress,
} from './requirements.service';
import { AuthUser } from '../auth/auth.service';

@Controller('requirements')
@UseGuards(AuthGuard('jwt'))
export class RequirementsController {
  constructor(private readonly service: RequirementsService) {}

  @Get()
  list(
    @Query('status') status?: string,
    @Query('keyword') keyword?: string,
  ): Requirement[] {
    return this.service.list(status, keyword);
  }

  @Get(':id')
  detail(@Param('id') id: string): Requirement {
    return this.service.detail(Number(id));
  }

  @Post()
  create(
    @Request() req: { user: AuthUser },
    @Body()
    body: {
      title?: string;
      description?: string;
      status?: string;
      priority?: string;
      owner?: string;
      startDate?: string;
      dueDate?: string;
    },
  ): Requirement {
    return this.service.create(body, req.user.id);
  }

  @Put(':id')
  update(
    @Param('id') id: string,
    @Body()
    body: {
      title?: string;
      description?: string;
      status?: string;
      priority?: string;
      owner?: string;
      startDate?: string;
      dueDate?: string;
    },
  ): Requirement {
    return this.service.update(Number(id), body);
  }

  @Delete(':id')
  delete(@Param('id') id: string): { message: string } {
    return this.service.delete(Number(id));
  }

  @Get(':id/progress')
  progress(@Param('id') id: string): RequirementProgress[] {
    return this.service.progress(Number(id));
  }

  @Post(':id/progress')
  addProgress(
    @Param('id') id: string,
    @Body()
    body: {
      dept?: string;
      person?: string;
      status?: string;
      progressDate?: string;
      content?: string;
    },
  ): RequirementProgress[] {
    return this.service.addProgress(Number(id), body);
  }

  @Put('progress/:progressId')
  updateProgress(
    @Param('progressId') progressId: string,
    @Body()
    body: {
      dept?: string;
      person?: string;
      status?: string;
      progressDate?: string;
      content?: string;
    },
  ): { message: string } {
    return this.service.updateProgress(Number(progressId), body);
  }

  @Delete('progress/:progressId')
  deleteProgress(@Param('progressId') progressId: string): { message: string } {
    return this.service.deleteProgress(Number(progressId));
  }
}
