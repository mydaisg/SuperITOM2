"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.AuthService = void 0;
const common_1 = require("@nestjs/common");
const jwt_1 = require("@nestjs/jwt");
const database_service_1 = require("../../common/database/database.service");
let AuthService = class AuthService {
    constructor(db, jwtService) {
        this.db = db;
        this.jwtService = jwtService;
    }
    toUser(row) {
        return {
            id: row.id,
            username: row.username,
            role: row.role,
            displayName: row.display_name || row.username,
        };
    }
    async validateUser(userId) {
        const row = this.db.get('SELECT id, username, role, COALESCE(display_name, username) AS display_name FROM users WHERE id = ?', [userId]);
        return row ? this.toUser(row) : null;
    }
    async login(loginDto) {
        const { username, password } = loginDto;
        const row = this.db.get('SELECT id, username, password, role, active, COALESCE(display_name, username) AS display_name FROM users WHERE username = ?', [username]);
        if (!row || row.password !== password) {
            throw new common_1.UnauthorizedException('用户名或密码错误');
        }
        if (row.active === 0) {
            throw new common_1.UnauthorizedException('您的账号已被禁用，请联系管理员');
        }
        const user = this.toUser(row);
        const accessToken = this.jwtService.sign({
            sub: user.id,
            username: user.username,
            role: user.role,
        });
        return { accessToken, user };
    }
    async register(registerDto) {
        const { username, password, role, displayName } = registerDto;
        const exists = this.db.get('SELECT id FROM users WHERE username = ?', [
            username,
        ]);
        if (exists) {
            throw new common_1.ConflictException('用户名已存在');
        }
        const info = this.db.run("INSERT INTO users (username, password, role, display_name, active, created_at, updated_at) VALUES (?, ?, ?, ?, 1, datetime('now','localtime'), datetime('now','localtime'))", [username, password, role || 'user', displayName || '']);
        return {
            id: Number(info.lastInsertRowid),
            username,
            role: role || 'user',
            displayName: displayName || '',
        };
    }
    async getUserById(id) {
        const user = await this.validateUser(id);
        if (!user) {
            throw new common_1.UnauthorizedException('用户不存在');
        }
        return user;
    }
};
exports.AuthService = AuthService;
exports.AuthService = AuthService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [database_service_1.DatabaseService,
        jwt_1.JwtService])
], AuthService);
//# sourceMappingURL=auth.service.js.map