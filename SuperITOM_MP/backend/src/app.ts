import express from 'express';
import type { Request, Response, NextFunction } from 'express';
import authRouter from './routes/auth.js';
import homeRouter from './routes/home.js';
import workordersRouter from './routes/workorders.js';

export function createApp(): express.Express {
  const app = express();
  app.use(express.json());

  app.get('/health', (_req, res) => {
    res.json({ status: 'ok', time: new Date().toISOString() });
  });

  app.use('/api/auth', authRouter);
  app.use('/api/home', homeRouter);
  app.use('/api/workorders', workordersRouter);

  // 404
  app.use((_req, res) => {
    res.status(404).json({ success: false, message: '接口不存在' });
  });

  // 统一错误处理
  app.use((err: Error, _req: Request, res: Response, _next: NextFunction) => {
    console.error('[error]', err);
    res.status(500).json({ success: false, message: err.message || '服务器内部错误' });
  });

  return app;
}
