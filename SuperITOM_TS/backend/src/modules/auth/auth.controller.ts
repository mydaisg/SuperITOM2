import { Controller, Post, Get, Body, UseGuards, Request } from '@nestjs/common';
import { AuthGuard } from '@nestjs/passport';
import { AuthService, AuthUser } from './auth.service';
import { LoginDto } from './dto/login.dto';
import { RegisterDto } from './dto/register.dto';

@Controller('auth')
export class AuthController {
  constructor(private authService: AuthService) {}

  @Post('login')
  async login(@Body() loginDto: LoginDto): Promise<{ accessToken: string; user: AuthUser }> {
    return this.authService.login(loginDto);
  }

  @Post('register')
  async register(@Body() registerDto: RegisterDto): Promise<AuthUser> {
    return this.authService.register(registerDto);
  }

  @UseGuards(AuthGuard('jwt'))
  @Get('profile')
  async getProfile(@Request() req: { user: { id: number } }): Promise<AuthUser> {
    return this.authService.getUserById(req.user.id);
  }

  @UseGuards(AuthGuard('jwt'))
  @Get('logout')
  async logout(): Promise<{ message: string }> {
    return { message: '注销成功' };
  }
}
