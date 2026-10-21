import { Repository } from 'typeorm';
import { UserEntity } from './entities/user.entity';
import { JwtService } from '@nestjs/jwt';
import { LoginDto } from './dto/login.dto';
import { RegisterDto } from './dto/register.dto';
import { UserDto } from './dto/user.dto';
export declare class AuthService {
    private userRepository;
    private jwtService;
    constructor(userRepository: Repository<UserEntity>, jwtService: JwtService);
    validateUser(userId: number): Promise<UserEntity | null>;
    login(loginDto: LoginDto): Promise<{
        accessToken: string;
        user: UserDto;
    }>;
    register(registerDto: RegisterDto): Promise<UserDto>;
    getUserById(id: number): Promise<UserDto>;
    private mapUserToDto;
}
