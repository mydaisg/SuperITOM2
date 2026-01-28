import { Controller, Get, Post, Body, Param, UseGuards } from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';
import { AutomationService } from './automation.service';
import { ScriptDto } from './dto/script.dto';
import { ExecuteDto } from './dto/execute.dto';
import { Observable } from 'rxjs';

@Controller('automation')
@UseGuards(AuthGuard('jwt'))
export class AutomationController {
  constructor(private automationService: AutomationService) {}

  /**
   * 获取可用的 PowerShell 脚本
   */
  @Get('scripts')
  getScripts(): ScriptDto[] {
    return this.automationService.getAvailableScripts();
  }

  /**
   * 执行 PowerShell 脚本
   */
  @Post('execute')
  executeScript(@Body() executeDto: ExecuteDto): Observable<string> {
    return this.automationService.executeScript(executeDto);
  }

  /**
   * 终止脚本执行
   */
  @Post('terminate/:id')
  terminateScript(@Param('id') id: string): { success: boolean } {
    const success = this.automationService.terminateScript(id);
    return { success };
  }

  /**
   * 获取脚本执行状态
   */
  @Get('status/:id')
  getExecutionStatus(@Param('id') id: string): { running: boolean } {
    const running = this.automationService.getExecutionStatus(id);
    return { running };
  }
}