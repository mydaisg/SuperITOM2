import { AutomationService } from './automation.service';
import { ScriptDto } from './dto/script.dto';
import { ExecuteDto } from './dto/execute.dto';
import { Observable } from 'rxjs';
export declare class AutomationController {
    private automationService;
    constructor(automationService: AutomationService);
    getScripts(): ScriptDto[];
    executeScript(executeDto: ExecuteDto): Observable<string>;
    terminateScript(id: string): {
        success: boolean;
    };
    getExecutionStatus(id: string): {
        running: boolean;
    };
}
