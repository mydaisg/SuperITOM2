import { Router } from 'express';
import jwt from 'jsonwebtoken';
import { queryOne, type UserRow } from '../db.js';
import { config } from '../config.js';

const router = Router();

// 登录：沿用现有 auth.r 的明文密码比对逻辑，保证与 R 端账号数据完全兼容
router.post('/login', (req, res) => {
  const { username, password } = (req.body || {}) as { username?: string; password?: string };

  if (!username || !password) {
    res.status(400).json({ success: false, message: '请输入用户名和密码' });
    return;
  }

  const user = queryOne<UserRow>(
    'SELECT * FROM users WHERE username = ? AND password = ?',
    username,
    password,
  );

  if (!user) {
    res.status(401).json({ success: false, message: '用户名或密码错误' });
    return;
  }

  if (user.active === 0) {
    res.status(403).json({ success: false, message: '您的账号已被禁用，请联系管理员' });
    return;
  }

  const token = jwt.sign(
    { sub: user.id, username: user.username, role: user.role },
    config.jwtSecret,
    { expiresIn: config.jwtExpiresIn } as jwt.SignOptions,
  );

  res.json({
    success: true,
    token,
    user: {
      id: user.id,
      username: user.username,
      displayName: user.display_name || user.username,
      role: user.role,
    },
  });
});

export default router;
