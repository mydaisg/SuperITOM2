App<IAppOption>({
  globalData: {
    userInfo: null,
    token: '',
  },

  onLaunch() {
    // 启动时恢复登录态
    const token = wx.getStorageSync('token');
    if (token) {
      this.globalData.token = token;
      const userInfo = wx.getStorageSync('userInfo');
      if (userInfo) {
        this.globalData.userInfo = userInfo as AppUserInfo;
      }
    }
  },
});
