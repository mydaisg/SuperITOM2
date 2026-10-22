import { AuthService, AuthUser } from './auth.service';
import { LoginDto } from './dto/login.dto';
import { RegisterDto } from './dto/register.dto';
export declare class AuthController {
    private authService;
    constructor(authService: AuthService);
    login(loginDto: LoginDto): Promise<{
        accessToken: string;
        user: AuthUser;
    }>;
    register(registerDto: RegisterDto): Promise<AuthUser>;
    getProfile(req: {
        user: {
            id: number;
        };
    }): Promise<AuthUser>;
    logout(): Promise<{
        message: string;
    }>;
}
