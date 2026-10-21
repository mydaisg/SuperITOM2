import { request } from '../../utils/request';
import { getToken, getUserInfo, clearAuth } from '../../utils/auth';
import {
  PROJECT_STATUS,
  WORK_ORDER_STATUS,
  TASK_STATUS,
  statusMeta,
} from '../../utils/status';

interface WoStats {
  total: number;
  pending: number;
  assigned: number;
  processing: number;
  completed: number;
  closed: number;
}

interface StatCard {
  key: string;
  label: string;
  value: number;
  color: string;
}

interface ProjectItem {
  id: number;
  project_no: string;
  name: string;
  status: string;
  priority: string;
  statusText: string;
  statusColor: string;
}

interface WorkOrderItem {
  id: number;
  order_no: string;
  title: string;
  status: string;
  priority: string;
  category: string;
  statusText: string;
  statusColor: string;
}

interface TaskItem {
  id: number;
  task_no: string;
  name: string;
  status: string;
  priority: string;
  importance: number;
  project_name: string;
  importanceFlags: string;
  statusText: string;
  statusColor: string;
}

interface IndexData {
  displayName: string;
  stats: WoStats;
  statCards: StatCard[];
  projects: ProjectItem[];
  workOrders: WorkOrderItem[];
  tasks: TaskItem[];
  loading: boolean;
}

const EMPTY_STATS: WoStats = {
  total: 0,
  pending: 0,
  assigned: 0,
  processing: 0,
  completed: 0,
  closed: 0,
};

Page({
  data: {
    displayName: '',
    stats: EMPTY_STATS,
    statCards: [],
    projects: [],
    workOrders: [],
    tasks: [],
    loading: true,
  } as IndexData,

  onLoad() {
    if (!getToken()) {
      wx.reLaunch({ url: '/pages/login/login' });
      return;
    }
    const user = getUserInfo();
    this.setData({ displayName: user ? user.displayName : '' });
    this.loadDashboard();
  },

  onPullDownRefresh() {
    this.loadDashboard().finally(() => wx.stopPullDownRefresh());
  },

  async loadDashboard() {
    this.setData({ loading: true });
    try {
      const res = await request<any>('/api/home/dashboard');
      const d = res.data || {};
      const stats: WoStats = { ...EMPTY_STATS, ...(d.stats || {}) };

      const statDefs = [
        { key: 'total', label: '全部', color: '#667eea' },
        { key: 'pending', label: '待处理', color: '#f0ad4e' },
        { key: 'assigned', label: '已派发', color: '#5bc0de' },
        { key: 'processing', label: '处理中', color: '#337ab7' },
        { key: 'completed', label: '已完成', color: '#5cb85c' },
        { key: 'closed', label: '已关闭', color: '#d9534f' },
      ];
      const statCards: StatCard[] = statDefs.map((s) => ({
        ...s,
        value: Number((stats as any)[s.key]) || 0,
      }));

      this.setData({
        stats,
        statCards,
        projects: (d.projects || []).map((p: any) => ({
          ...p,
          statusText: statusMeta(PROJECT_STATUS, p.status).text,
          statusColor: statusMeta(PROJECT_STATUS, p.status).color,
        })),
        workOrders: (d.workOrders || []).map((w: any) => ({
          ...w,
          statusText: statusMeta(WORK_ORDER_STATUS, w.status).text,
          statusColor: statusMeta(WORK_ORDER_STATUS, w.status).color,
        })),
        tasks: (d.tasks || []).map((t: any) => ({
          ...t,
          importance: Number(t.importance) || 0,
          importanceFlags: '🚩'.repeat(Number(t.importance) || 0),
          statusText: statusMeta(TASK_STATUS, t.status).text,
          statusColor: statusMeta(TASK_STATUS, t.status).color,
        })),
      });
    } catch (err) {
      wx.showToast({ title: (err as Error).message || '加载失败', icon: 'none' });
    } finally {
      this.setData({ loading: false });
    }
  },

  onGoWorkorders() {
    wx.navigateTo({ url: '/pages/workorders/list' });
  },

  onLogout() {
    wx.showModal({
      title: '提示',
      content: '确定退出登录吗？',
      success: (res) => {
        if (res.confirm) {
          clearAuth();
          wx.reLaunch({ url: '/pages/login/login' });
        }
      },
    });
  },
});
