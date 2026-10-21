/// <reference types="miniprogram-api-typings" />

// 全局数据声明
interface AppUserInfo {
  id: number;
  username: string;
  displayName: string;
  role: string;
}

interface SuperItomGlobalData {
  userInfo: AppUserInfo | null;
  token: string;
}

interface IAppOption {
  globalData: SuperItomGlobalData;
}
