import { Injectable, NotFoundException } from '@nestjs/common';
import { Observable, Subject } from 'rxjs';
import * as pty from 'node-pty';
import * as path from 'path';
import * as fs from 'fs';
import { ScriptDto } from './dto/script.dto';
import { ExecuteDto } from './dto/execute.dto';
import { ExecuteResultDto } from './dto/execute-result.dto';

@Injectable()
export class AutomationService {
  private scriptExecutions = new Map<string, { pty: pty.IPty; output: Subject<string> }>();

  constructor() {}

  /**
   * 获取可用的 PowerShell 脚本
   */
  getAvailableScripts(): ScriptDto[] {
    const scriptsDir = path.join(__dirname, '..', '..', '..', '..', 'STD');
    
    if (!fs.existsSync(scriptsDir)) {
      return [];
    }

    const files = fs.readdirSync(scriptsDir);
    const scripts = files.filter(file => file.endsWith('.ps1'));

    return scripts.map(script => ({
      name: script,
      path: path.join(scriptsDir, script),
    }));
  }

  /**
   * 执行 PowerShell 脚本
   */
  executeScript(executeDto: ExecuteDto): Observable<string> {
    const { script, args = [] } = executeDto;
    const scriptsDir = path.join(__dirname, '..', '..', '..', '..', 'STD');
    const scriptPath = path.join(scriptsDir, script);

    if (!fs.existsSync(scriptPath)) {
      throw new NotFoundException(`脚本 ${script} 不存在`);
    }

    const executionId = this.generateExecutionId();
    const outputSubject = new Subject<string>();

    try {
      const ptyProcess = pty.spawn('powershell.exe', ['-File', scriptPath, ...args], {
        name: 'xterm-color',
        cols: 80,
        rows: 30,
        cwd: process.cwd(),
        env: process.env,
      });

      this.scriptExecutions.set(executionId, { pty: ptyProcess, output: outputSubject });

      ptyProcess.on('data', (data) => {
        outputSubject.next(data);
      });

      ptyProcess.on('exit', (code) => {
        outputSubject.complete();
        this.scriptExecutions.delete(executionId);
      });

      ptyProcess.on('error', (error) => {
        outputSubject.error(error);
        this.scriptExecutions.delete(executionId);
      });

      return outputSubject.asObservable();
    } catch (error) {
      outputSubject.error(error);
      return outputSubject.asObservable();
    }
  }

  /**
   * 终止脚本执行
   */
  terminateScript(executionId: string): boolean {
    const execution = this.scriptExecutions.get(executionId);
    if (execution) {
      execution.pty.kill();
      this.scriptExecutions.delete(executionId);
      return true;
    }
    return false;
  }

  /**
   * 获取脚本执行状态
   */
  getExecutionStatus(executionId: string): boolean {
    return this.scriptExecutions.has(executionId);
  }

  /**
   * 生成唯一的执行 ID
   */
  private generateExecutionId(): string {
    return `exec_${Date.now()}_${Math.floor(Math.random() * 10000)}`;
  }
}