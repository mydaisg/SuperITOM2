import axios from 'axios'

// 统一 API 实例：自动携带 JWT token
export const api = axios.create({
  baseURL: 'http://localhost:3000',
})

api.interceptors.request.use((config) => {
  const token = localStorage.getItem('token')
  if (token) {
    config.headers.Authorization = `Bearer ${token}`
  }
  return config
})

export interface WorkOrderListItem {
  id: number
  orderNo: string
  title: string
  priority: string
  status: string
  category: string
  creatorName: string
  assigneeName: string
  createdAt: string
}

export interface WorkOrderDetail {
  id: number
  orderNo: string
  title: string
  description: string
  priority: string
  status: string
  category: string
  subcategory: string
  requestUser: string
  resolution: string
  closeReason: string
  creatorId: number
  assigneeId: number | null
  handlerId: number | null
  creatorName: string
  assigneeName: string
  handlerName: string
  createdAt: string
  assignedAt: string
  completedAt: string
  closedAt: string
  commentCount: number
}

export interface WorkOrderComment {
  id: number
  comment: string
  createdAt: string
  creatorName: string
}

export interface AssignableUser {
  id: number
  username: string
  role: string
  displayName: string
}

export interface CurrentUser {
  id: number
  username: string
  role: string
  displayName?: string
}

export const STATUS_META: Record<string, { label: string; color: string }> = {
  pending: { label: '待处理', color: 'orange' },
  assigned: { label: '已派发', color: 'cyan' },
  processing: { label: '处理中', color: 'blue' },
  completed: { label: '已完成', color: 'green' },
  closed: { label: '已关闭', color: 'red' },
}

export const PRIORITY_COLOR: Record<string, string> = {
  紧急: 'red',
  高: 'orange',
  中: 'blue',
  低: 'gray',
}

export const PRIORITIES = ['低', '中', '高', '紧急']
export const CATEGORIES = ['一般', '故障', '请求', '咨询', '变更', '其他']

export const STATUS_FILTERS = [
  { value: 'all', label: '全部' },
  { value: 'pending', label: '待处理' },
  { value: 'assigned', label: '已派发' },
  { value: 'processing', label: '处理中' },
  { value: 'completed', label: '已完成' },
  { value: 'closed', label: '已关闭' },
]

export function currentUser(): CurrentUser {
  try {
    return JSON.parse(localStorage.getItem('user') || '{}') as CurrentUser
  } catch {
    return { id: 0, username: '', role: '' }
  }
}

export function canAssign(): boolean {
  const u = currentUser()
  return u.role === 'admin' || u.role === 'it_desk'
}
