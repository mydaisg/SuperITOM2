import { BASE_URL } from './config';
import { getToken, clearAuth } from './auth';

export interface ApiResponse<T = unknown> {
  success: boolean;
  message?: string;
  data?: T;
  token?: string;
  user?: any;
}

type Method = 'GET' | 'POST' | 'PUT' | 'DELETE';

export function request<T = unknown>(
  path: string,
  options: { method?: Method; data?: any } = {},
): Promise<ApiResponse<T>> {
  const token = getToken();

  return new Promise((resolve, reject) => {
    wx.request({
      url: `${BASE_URL}${path}`,
      method: options.method || 'GET',
      data: options.data,
      header: {
        'Content-Type': 'application/json',
        ...(token ? { Authorization: `Bearer ${token}` } : {}),
      },
      success(res) {
        if (res.statusCode === 401) {
          clearAuth();
          wx.reLaunch({ url: '/pages/login/login' });
          reject(new Error('登录已过期，请重新登录'));
          return;
        }

        const body = res.data as ApiResponse<T>;
        if (body && body.success === false) {
          reject(new Error(body.message || '请求失败'));
          return;
        }
        resolve(body);
      },
      fail(err) {
        reject(new Error(err.errMsg || '网络错误'));
      },
    });
  });
}
