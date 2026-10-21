export interface StatusMeta {
  text: string;
  color: string;
}

export const PROJECT_STATUS: Record<string, StatusMeta> = {
  planning: { text: '规划中', color: '#5bc0de' },
  active: { text: '进行中', color: '#337ab7' },
  suspended: { text: '已暂停', color: '#f0ad4e' },
  completed: { text: '已完成', color: '#5cb85c' },
  closed: { text: '已关闭', color: '#d9534f' },
};

export const WORK_ORDER_STATUS: Record<string, StatusMeta> = {
  pending: { text: '待处理', color: '#f0ad4e' },
  assigned: { text: '已派发', color: '#5bc0de' },
  processing: { text: '处理中', color: '#337ab7' },
  completed: { text: '已完成', color: '#5cb85c' },
  closed: { text: '已关闭', color: '#d9534f' },
};

export const TASK_STATUS: Record<string, StatusMeta> = {
  pending: { text: '待处理', color: '#f0ad4e' },
  in_progress: { text: '进行中', color: '#337ab7' },
  blocked: { text: '已阻塞', color: '#d9534f' },
  completed: { text: '已完成', color: '#5cb85c' },
};

export function statusMeta(map: Record<string, StatusMeta>, key: string): StatusMeta {
  return map[key] || { text: key || '-', color: '#999' };
}

export function priorityColor(p: string): string {
  switch (p) {
    case '紧急':
      return '#d9534f';
    case '高':
      return '#f0ad4e';
    case '中':
      return '#5bc0de';
    case '低':
      return '#999';
    default:
      return '#999';
  }
}
