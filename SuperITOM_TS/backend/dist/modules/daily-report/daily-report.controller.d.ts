import { DailyReportService, DailyReport } from './daily-report.service';
import { AuthUser } from '../auth/auth.service';
export declare class DailyReportController {
    private readonly service;
    constructor(service: DailyReportService);
    getReport(req: {
        user: AuthUser;
    }, date?: string): DailyReport;
}
