import { request } from '../../utils/request';
import { getToken, getUserInfo } from '../../utils/auth';
import { WORK_ORDER_STATUS, statusMeta, priorityColor } from '../../utils/status';

interface Comment {
  id: number;
  comment: string;
  created_at: string;
  creator_name: string;
}

interface AssignableUser {
  id: number;
  username: string;
  display_name: string;
  role: string;
}

Page({
  data: {
    id: 0,
    wo: null as any,
    comments: [] as Comment[],
    commentInput: '',
    userId: 0,
    canAssign: false,
    isAssignee: false,
    isHandler: false,
    assignableUsers: [] as AssignableUser[],
    assignableNames: [] as string[],
    assignIndex: 0,
    loading: true,
    sending: false,
  },

  onLoad(options: Record<string, string>) {
    if (!getToken()) {
      wx.reLaunch({ url: '/pages/login/login' });
      return;
    }
    const user = getUserInfo();
    this.setData({
      id: Number(options.id),
      userId: user?.id || 0,
      canAssign: user?.role === 'admin' || user?.role === 'it_desk',
    });
    this.loadDetail();
    this.loadComments();
    if (this.data.canAssign) {
      this.loadAssignableUsers();
    }
  },

  async loadDetail(): Promise<void> {
    this.setData({ loading: true });
    try {
      const res = await request<any>(`/api/workorders/${this.data.id}`);
      const wo = res.data;
      if (wo) {
        const sm = statusMeta(WORK_ORDER_STATUS, wo.status);
        this.setData({
          wo: { ...wo, statusText: sm.text, statusColor: sm.color, priorityColor: priorityColor(wo.priority) },
          isAssignee: wo.assigned_to === this.data.userId,
          isHandler: wo.handled_by === this.data.userId,
        });
      }
    } catch (err) {
      wx.showToast({ title: (err as Error).message || '加载失败', icon: 'none' });
    } finally {
      this.setData({ loading: false });
    }
  },

  async loadComments(): Promise<void> {
    try {
      const res = await request<any>(`/api/workorders/${this.data.id}/comments`);
      this.setData({ comments: res.data || [] });
    } catch {
      // 评论加载失败不阻塞主流程
    }
  },

  async loadAssignableUsers(): Promise<void> {
    try {
      const res = await request<any>('/api/workorders/assignable-users');
      const users: AssignableUser[] = res.data || [];
      this.setData({
        assignableUsers: users,
        assignableNames: users.map((u) => u.display_name || u.username),
      });
    } catch {
      // 忽略
    }
  },

  onAssignPicker(e: WechatMiniprogram.PickerChange) {
    this.setData({ assignIndex: Number(e.detail.value) });
  },

  async onAssign(): Promise<void> {
    const u = this.data.assignableUsers[this.data.assignIndex];
    if (!u) {
      wx.showToast({ title: '请先选择派发对象', icon: 'none' });
      return;
    }
    try {
      await request(`/api/workorders/${this.data.id}/assign`, {
        method: 'POST',
        data: { assigned_to: u.id },
      });
      wx.showToast({ title: '派发成功', icon: 'success' });
      this.loadDetail();
    } catch (err) {
      wx.showToast({ title: (err as Error).message, icon: 'none' });
    }
  },

  async onStart(): Promise<void> {
    try {
      await request(`/api/workorders/${this.data.id}/start`, { method: 'POST' });
      wx.showToast({ title: '已开始处理', icon: 'success' });
      this.loadDetail();
    } catch (err) {
      wx.showToast({ title: (err as Error).message, icon: 'none' });
    }
  },

  onComplete() {
    wx.showModal({
      title: '完成工单',
      editable: true,
      placeholderText: '请输入解决方案/处理结果',
      success: async (res) => {
        if (!res.confirm) return;
        try {
          await request(`/api/workorders/${this.data.id}/complete`, {
            method: 'POST',
            data: { resolution: res.content || '' },
          });
          wx.showToast({ title: '已完成', icon: 'success' });
          this.loadDetail();
        } catch (err) {
          wx.showToast({ title: (err as Error).message, icon: 'none' });
        }
      },
    });
  },

  onClose() {
    wx.showModal({
      title: '关闭工单',
      editable: true,
      placeholderText: '关闭原因（可选）',
      success: async (res) => {
        if (!res.confirm) return;
        try {
          await request(`/api/workorders/${this.data.id}/close`, {
            method: 'POST',
            data: { close_reason: res.content || '' },
          });
          wx.showToast({ title: '已关闭', icon: 'success' });
          this.loadDetail();
        } catch (err) {
          wx.showToast({ title: (err as Error).message, icon: 'none' });
        }
      },
    });
  },

  onCommentInput(e: WechatMiniprogram.Input) {
    this.setData({ commentInput: e.detail.value });
  },

  async onSendComment(): Promise<void> {
    const content = this.data.commentInput.trim();
    if (!content) return;
    this.setData({ sending: true });
    try {
      await request(`/api/workorders/${this.data.id}/comments`, {
        method: 'POST',
        data: { comment: content },
      });
      this.setData({ commentInput: '' });
      this.loadComments();
    } catch (err) {
      wx.showToast({ title: (err as Error).message, icon: 'none' });
    } finally {
      this.setData({ sending: false });
    }
  },
});
