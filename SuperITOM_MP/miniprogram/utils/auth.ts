export interface UserInfo {
  id: number;
  username: string;
  displayName: string;
  role: string;
}

export function getToken(): string {
  return (wx.getStorageSync('token') as string) || '';
}

export function setToken(token: string): void {
  wx.setStorageSync('token', token);
}

export function getUserInfo(): UserInfo | null {
  const u = wx.getStorageSync('userInfo');
  return u ? (u as UserInfo) : null;
}

export function setUserInfo(user: UserInfo): void {
  wx.setStorageSync('userInfo', user);
}

export function clearAuth(): void {
  wx.removeStorageSync('token');
  wx.removeStorageSync('userInfo');
}
