import { request } from '../../utils/request';
import { setToken, setUserInfo, type UserInfo } from '../../utils/auth';

interface LoginData {
  username: string;
  password: string;
  loading: boolean;
}

Page({
  data: {
    username: '',
    password: '',
    loading: false,
  } as LoginData,

  onUsernameInput(e: WechatMiniprogram.Input) {
    this.setData({ username: e.detail.value });
  },

  onPasswordInput(e: WechatMiniprogram.Input) {
    this.setData({ password: e.detail.value });
  },

  async onLogin() {
    const { username, password } = this.data;
    if (!username.trim() || !password) {
      wx.showToast({ title: '请输入用户名和密码', icon: 'none' });
      return;
    }

    this.setData({ loading: true });
    try {
      const res = await request<never>('/api/auth/login', {
        method: 'POST',
        data: { username: username.trim(), password },
      });

      setToken(res.token as string);
      setUserInfo(res.user as UserInfo);

      wx.showToast({ title: '登录成功', icon: 'success' });
      wx.reLaunch({ url: '/pages/index/index' });
    } catch (err) {
      wx.showToast({ title: (err as Error).message || '登录失败', icon: 'none' });
    } finally {
      this.setData({ loading: false });
    }
  },
});
