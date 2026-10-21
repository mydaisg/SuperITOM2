import { AuthService } from './auth.service';
import { LoginDto } from './dto/login.dto';
import { RegisterDto } from './dto/register.dto';
import { UserDto } from './dto/user.dto';
export declare class AuthController {
    private authService;
    constructor(authService: AuthService);
    login(loginDto: LoginDto): Promise<{
        accessToken: string;
        user: UserDto;
    }>;
    register(registerDto: RegisterDto): Promise<UserDto>;
    getProfile(req: {
        user: {
            id: number;
        };
    }): Promise<UserDto>;
    logout(): Promise<{
        message: string;
    }>;
}
