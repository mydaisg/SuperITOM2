import type { Request, Response, NextFunction } from 'express';
import jwt from 'jsonwebtoken';
import { config } from '../config.js';

export interface JwtUser {
  sub: number;
  username: string;
  role: string;
}

declare global {
  namespace Express {
    interface Request {
      user?: JwtUser;
    }
  }
}

export function authRequired(req: Request, res: Response, next: NextFunction): void {
  const header = req.headers.authorization;
  if (!header || !header.startsWith('Bearer ')) {
    res.status(401).json({ success: false, message: '未登录' });
    return;
  }

  const token = header.slice('Bearer '.length);
  try {
    const payload = jwt.verify(token, config.jwtSecret) as unknown as JwtUser & { exp?: number };
    req.user = { sub: payload.sub, username: payload.username, role: payload.role };
    next();
  } catch {
    res.status(401).json({ success: false, message: '登录已过期，请重新登录' });
  }
}
