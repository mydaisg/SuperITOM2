import {
  Controller,
  Get,
  Post,
  Body,
  Param,
  Query,
  Request,
  UseGuards,
  ParseIntPipe,
} from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';
import { WorkOrdersService, AuthUser } from './work-orders.service';
import { CreateWorkOrderDto } from './dto/create-work-order.dto';
import { AssignWorkOrderDto } from './dto/assign-work-order.dto';
import { CompleteWorkOrderDto } from './dto/complete-work-order.dto';
import { CloseWorkOrderDto } from './dto/close-work-order.dto';
import { AddCommentDto } from './dto/add-comment.dto';

@Controller('work-orders')
@UseGuards(AuthGuard('jwt'))
export class WorkOrdersController {
  constructor(private readonly workOrdersService: WorkOrdersService) {}

  @Get()
  list(@Request() req: { user: AuthUser }, @Query('status') status?: string) {
    return this.workOrdersService.list(req.user, status);
  }

  @Get('assignable-users')
  assignableUsers() {
    return this.workOrdersService.assignableUsers();
  }

  @Post()
  create(@Request() req: { user: AuthUser }, @Body() dto: CreateWorkOrderDto) {
    return this.workOrdersService.create(req.user, dto);
  }

  @Get(':id')
  detail(@Param('id', ParseIntPipe) id: number) {
    return this.workOrdersService.detail(id);
  }

  @Get(':id/comments')
  comments(@Param('id', ParseIntPipe) id: number) {
    return this.workOrdersService.comments(id);
  }

  @Post(':id/comments')
  addComment(
    @Request() req: { user: AuthUser },
    @Param('id', ParseIntPipe) id: number,
    @Body() dto: AddCommentDto,
  ) {
    return this.workOrdersService.addComment(req.user, id, dto);
  }

  @Post(':id/assign')
  assign(
    @Request() req: { user: AuthUser },
    @Param('id', ParseIntPipe) id: number,
    @Body() dto: AssignWorkOrderDto,
  ) {
    return this.workOrdersService.assign(req.user, id, dto);
  }

  @Post(':id/start')
  start(@Request() req: { user: AuthUser }, @Param('id', ParseIntPipe) id: number) {
    return this.workOrdersService.start(req.user, id);
  }

  @Post(':id/complete')
  complete(
    @Request() req: { user: AuthUser },
    @Param('id', ParseIntPipe) id: number,
    @Body() dto: CompleteWorkOrderDto,
  ) {
    return this.workOrdersService.complete(req.user, id, dto);
  }

  @Post(':id/close')
  close(
    @Request() req: { user: AuthUser },
    @Param('id', ParseIntPipe) id: number,
    @Body() dto: CloseWorkOrderDto,
  ) {
    return this.workOrdersService.close(req.user, id, dto);
  }
}
