import { Observable } from 'rxjs';
import { ScriptDto } from './dto/script.dto';
import { ExecuteDto } from './dto/execute.dto';
export declare class AutomationService {
    private scriptExecutions;
    constructor();
    getAvailableScripts(): ScriptDto[];
    executeScript(executeDto: ExecuteDto): Observable<string>;
    terminateScript(executionId: string): boolean;
    getExecutionStatus(executionId: string): boolean;
    private generateExecutionId;
}
