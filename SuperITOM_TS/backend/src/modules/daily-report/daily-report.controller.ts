import { Controller, Get, Query, Request, UseGuards } from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';
import { DailyReportService, DailyReport } from './daily-report.service';
import { AuthUser } from '../auth/auth.service';

@Controller('daily-report')
@UseGuards(AuthGuard('jwt'))
export class DailyReportController {
  constructor(private readonly service: DailyReportService) {}

  @Get()
  getReport(
    @Request() req: { user: AuthUser },
    @Query('date') date?: string,
  ): DailyReport {
    const d = date || new Date().toISOString().slice(0, 10);
    return this.service.getReport(d, req.user);
  }
}
