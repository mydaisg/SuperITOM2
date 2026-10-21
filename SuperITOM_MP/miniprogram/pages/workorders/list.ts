import { request } from '../../utils/request';
import { getToken, getUserInfo } from '../../utils/auth';
import { WORK_ORDER_STATUS, statusMeta, priorityColor } from '../../utils/status';

const STATUS_TABS = [
  { key: 'all', label: '全部' },
  { key: 'pending', label: '待处理' },
  { key: 'assigned', label: '已派发' },
  { key: 'processing', label: '处理中' },
  { key: 'completed', label: '已完成' },
  { key: 'closed', label: '已关闭' },
];

interface WoItem {
  id: number;
  order_no: string;
  title: string;
  priority: string;
  status: string;
  category: string;
  creator_name: string;
  created_at: string;
  statusText: string;
  statusColor: string;
  priorityColor: string;
}

Page({
  data: {
    tabs: STATUS_TABS,
    activeTab: 'all',
    list: [] as WoItem[],
    total: 0,
    page: 1,
    pageSize: 20,
    loading: false,
    finished: false,
  },

  onLoad() {
    if (!getToken()) {
      wx.reLaunch({ url: '/pages/login/login' });
      return;
    }
    void getUserInfo();
    this.loadList(true);
  },

  onShow() {
    // 从详情/创建页返回时刷新列表
    if (this.data.list.length > 0) {
      this.loadList(true);
    }
  },

  onPullDownRefresh() {
    this.loadList(true).finally(() => wx.stopPullDownRefresh());
  },

  onReachBottom() {
    this.loadList(false);
  },

  onTabTap(e: WechatMiniprogram.TouchEvent) {
    const key = e.currentTarget.dataset.key as string;
    if (key === this.data.activeTab) return;
    this.setData({ activeTab: key });
    this.loadList(true);
  },

  async loadList(reset: boolean): Promise<void> {
    if (this.data.loading) return;
    if (!reset && this.data.finished) return;
    this.setData({ loading: true });

    const page = reset ? 1 : this.data.page + 1;
    try {
      const res = await request<any>(
        `/api/workorders?status=${this.data.activeTab}&page=${page}&pageSize=${this.data.pageSize}`,
      );
      const d = res.data || {};
      const items: WoItem[] = (d.list || []).map((w: any) => ({
        ...w,
        statusText: statusMeta(WORK_ORDER_STATUS, w.status).text,
        statusColor: statusMeta(WORK_ORDER_STATUS, w.status).color,
        priorityColor: priorityColor(w.priority),
      }));
      const list = reset ? items : this.data.list.concat(items);
      this.setData({
        list,
        total: d.total || 0,
        page,
        finished: list.length >= (d.total || 0),
      });
    } catch (err) {
      wx.showToast({ title: (err as Error).message || '加载失败', icon: 'none' });
    } finally {
      this.setData({ loading: false });
    }
  },

  onItemTap(e: WechatMiniprogram.TouchEvent) {
    const id = e.currentTarget.dataset.id as number;
    wx.navigateTo({ url: `/pages/workorders/detail?id=${id}` });
  },

  onCreate() {
    wx.navigateTo({ url: '/pages/workorders/create' });
  },
});
