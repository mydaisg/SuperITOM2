import {
  Injectable,
  UnauthorizedException,
  ConflictException,
} from '@nestjs/common';
import { JwtService } from '@nestjs/jwt';
import { DatabaseService } from '../../common/database/database.service';
import { LoginDto } from './dto/login.dto';
import { RegisterDto } from './dto/register.dto';

export interface AuthUser {
  id: number;
  username: string;
  role: string;
  displayName: string;
}

interface UserRow {
  id: number;
  username: string;
  password?: string;
  role: string;
  active?: number;
  display_name: string;
}

@Injectable()
export class AuthService {
  constructor(
    private readonly db: DatabaseService,
    private readonly jwtService: JwtService,
  ) {}

  private toUser(row: UserRow): AuthUser {
    return {
      id: row.id,
      username: row.username,
      role: row.role,
      displayName: row.display_name || row.username,
    };
  }

  async validateUser(userId: number): Promise<AuthUser | null> {
    const row = this.db.get<UserRow>(
      'SELECT id, username, role, COALESCE(display_name, username) AS display_name FROM users WHERE id = ?',
      [userId],
    );
    return row ? this.toUser(row) : null;
  }

  async login(loginDto: LoginDto): Promise<{ accessToken: string; user: AuthUser }> {
    const { username, password } = loginDto;
    const row = this.db.get<UserRow>(
      'SELECT id, username, password, role, active, COALESCE(display_name, username) AS display_name FROM users WHERE username = ?',
      [username],
    );
    if (!row || row.password !== password) {
      throw new UnauthorizedException('用户名或密码错误');
    }
    if (row.active === 0) {
      throw new UnauthorizedException('您的账号已被禁用，请联系管理员');
    }

    const user = this.toUser(row);
    const accessToken = this.jwtService.sign({
      sub: user.id,
      username: user.username,
      role: user.role,
    });
    return { accessToken, user };
  }

  async register(registerDto: RegisterDto): Promise<AuthUser> {
    const { username, password, role, displayName } = registerDto;
    const exists = this.db.get<{ id: number }>('SELECT id FROM users WHERE username = ?', [
      username,
    ]);
    if (exists) {
      throw new ConflictException('用户名已存在');
    }
    const info = this.db.run(
      "INSERT INTO users (username, password, role, display_name, active, created_at, updated_at) VALUES (?, ?, ?, ?, 1, datetime('now','localtime'), datetime('now','localtime'))",
      [username, password, role || 'user', displayName || ''],
    );
    return {
      id: Number(info.lastInsertRowid),
      username,
      role: role || 'user',
      displayName: displayName || '',
    };
  }

  async getUserById(id: number): Promise<AuthUser> {
    const user = await this.validateUser(id);
    if (!user) {
      throw new UnauthorizedException('用户不存在');
    }
    return user;
  }
}
