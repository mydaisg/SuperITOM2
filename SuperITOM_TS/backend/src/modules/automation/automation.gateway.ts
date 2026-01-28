import { WebSocketGateway, WebSocketServer, SubscribeMessage, OnGatewayInit, OnGatewayConnection, OnGatewayDisconnect } from '@nestjs/websockets';
import { Server, Socket } from 'socket.io';
import { AutomationService } from './automation.service';
import { ExecuteDto } from './dto/execute.dto';

@WebSocketGateway({
  cors: {
    origin: '*',
    methods: ['GET', 'POST'],
  },
})
export class AutomationGateway implements OnGatewayInit, OnGatewayConnection, OnGatewayDisconnect {
  @WebSocketServer()
  server: Server;

  constructor(private automationService: AutomationService) {}

  afterInit(server: Server) {
    console.log('WebSocket gateway initialized');
  }

  handleConnection(client: Socket) {
    console.log(`Client connected: ${client.id}`);
  }

  handleDisconnect(client: Socket) {
    console.log(`Client disconnected: ${client.id}`);
  }

  @SubscribeMessage('executeScript')
  async handleExecuteScript(client: Socket, executeDto: ExecuteDto) {
    try {
      const executionId = `exec_${Date.now()}_${client.id}`;
      
      // 执行脚本并发送实时输出
      const output$ = this.automationService.executeScript(executeDto);
      
      output$.subscribe(
        (data) => {
          client.emit('scriptOutput', { executionId, data });
        },
        (error) => {
          client.emit('scriptError', { executionId, error: error.message });
        },
        () => {
          client.emit('scriptComplete', { executionId });
        },
      );
      
      return { executionId, status: 'started' };
    } catch (error) {
      return { error: error.message };
    }
  }

  @SubscribeMessage('terminateScript')
  handleTerminateScript(client: Socket, executionId: string) {
    const success = this.automationService.terminateScript(executionId);
    return { success };
  }

  @SubscribeMessage('getScripts')
  handleGetScripts(client: Socket) {
    const scripts = this.automationService.getAvailableScripts();
    return { scripts };
  }
}