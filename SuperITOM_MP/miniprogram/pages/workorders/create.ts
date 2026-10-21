import { request } from '../../utils/request';
import { getToken } from '../../utils/auth';

const PRIORITIES = ['低', '中', '高', '紧急'];
const CATEGORIES = ['一般', '故障', '请求', '咨询', '变更', '其他'];

Page({
  data: {
    title: '',
    description: '',
    priorities: PRIORITIES,
    priorityIndex: 1,
    categories: CATEGORIES,
    categoryIndex: 0,
    subcategory: '',
    request_user: '',
    submitting: false,
  },

  onLoad() {
    if (!getToken()) {
      wx.reLaunch({ url: '/pages/login/login' });
      return;
    }
  },

  onTitleInput(e: WechatMiniprogram.Input) {
    this.setData({ title: e.detail.value });
  },

  onDescInput(e: WechatMiniprogram.TextareaInput) {
    this.setData({ description: e.detail.value });
  },

  onPriorityChange(e: WechatMiniprogram.PickerChange) {
    this.setData({ priorityIndex: Number(e.detail.value) });
  },

  onCategoryChange(e: WechatMiniprogram.PickerChange) {
    this.setData({ categoryIndex: Number(e.detail.value) });
  },

  onSubcategoryInput(e: WechatMiniprogram.Input) {
    this.setData({ subcategory: e.detail.value });
  },

  onRequestUserInput(e: WechatMiniprogram.Input) {
    this.setData({ request_user: e.detail.value });
  },

  async onSubmit(): Promise<void> {
    const title = this.data.title.trim();
    if (!title) {
      wx.showToast({ title: '请输入标题', icon: 'none' });
      return;
    }
    this.setData({ submitting: true });
    try {
      await request('/api/workorders', {
        method: 'POST',
        data: {
          title,
          description: this.data.description.trim(),
          priority: PRIORITIES[this.data.priorityIndex],
          category: CATEGORIES[this.data.categoryIndex],
          subcategory: this.data.subcategory.trim(),
          request_user: this.data.request_user.trim(),
        },
      });
      wx.showToast({ title: '创建成功', icon: 'success' });
      setTimeout(() => wx.navigateBack(), 1000);
    } catch (err) {
      wx.showToast({ title: (err as Error).message, icon: 'none' });
    } finally {
      this.setData({ submitting: false });
    }
  },
});
