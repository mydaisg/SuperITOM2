import { OnGatewayInit, OnGatewayConnection, OnGatewayDisconnect } from '@nestjs/websockets';
import { Server, Socket } from 'socket.io';
import { AutomationService } from './automation.service';
import { ExecuteDto } from './dto/execute.dto';
export declare class AutomationGateway implements OnGatewayInit, OnGatewayConnection, OnGatewayDisconnect {
    private automationService;
    server: Server;
    constructor(automationService: AutomationService);
    afterInit(server: Server): void;
    handleConnection(client: Socket): void;
    handleDisconnect(client: Socket): void;
    handleExecuteScript(client: Socket, executeDto: ExecuteDto): Promise<{
        executionId: string;
        status: string;
        error?: undefined;
    } | {
        error: any;
        executionId?: undefined;
        status?: undefined;
    }>;
    handleTerminateScript(client: Socket, executionId: string): {
        success: boolean;
    };
    handleGetScripts(client: Socket): {
        scripts: import("./dto/script.dto").ScriptDto[];
    };
}
