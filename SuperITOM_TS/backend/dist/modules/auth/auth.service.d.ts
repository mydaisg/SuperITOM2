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
export declare class AuthService {
    private readonly db;
    private readonly jwtService;
    constructor(db: DatabaseService, jwtService: JwtService);
    private toUser;
    validateUser(userId: number): Promise<AuthUser | null>;
    login(loginDto: LoginDto): Promise<{
        accessToken: string;
        user: AuthUser;
    }>;
    register(registerDto: RegisterDto): Promise<AuthUser>;
    getUserById(id: number): Promise<AuthUser>;
}
