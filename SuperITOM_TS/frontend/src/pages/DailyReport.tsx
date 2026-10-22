import React, { useState } from 'react'
import { Card, DatePicker, Button, Space, Tag, Empty, message, Spin } from 'antd'
import { ReloadOutlined, CopyOutlined } from '@ant-design/icons'
import dayjs, { Dayjs } from 'dayjs'
import { api } from '../services/workOrder'

interface WorkOrderItem {
  orderNo: string
  title: string
  status: string
  category: string
}
interface TaskItem {
  taskNo: string
  taskName: string
  status: string
  projectName: string
}
interface TaskLogItem {
  content: string
  creatorName: string
  createdAt: string
}
interface NoteCommentItem {
  content: string
  noteTitle: string
  status: string
}
interface UserReport {
  id: number
  username: string
  displayName: string
  workOrders: WorkOrderItem[]
  tasks: TaskItem[]
  taskLogs: TaskLogItem[]
  noteComments: NoteCommentItem[]
  counts: { wo: number; task: number; log: number; note: number }
}
interface DailyReport {
  date: string
  reports: UserReport[]
}

const WO_STATUS: Record<string, { label: string; color: string }> = {
  pending: { label: '待处理', color: 'orange' },
  assigned: { label: '已派发', color: 'cyan' },
  processing: { label: '处理中', color: 'blue' },
  completed: { label: '已完成', color: 'green' },
  closed: { label: '已关闭', color: 'red' },
}

const TASK_STATUS: Record<string, { label: string; color: string }> = {
  pending: { label: '待处理', color: 'orange' },
  in_progress: { label: '进行中', color: 'blue' },
  completed: { label: '已完成', color: 'green' },
  blocked: { label: '已阻塞', color: 'red' },
}

const DailyReport: React.FC = () => {
  const [date, setDate] = useState<Dayjs>(dayjs())
  const [report, setReport] = useState<DailyReport | null>(null)
  const [loading, setLoading] = useState(false)

  const fetchReport = async (d?: Dayjs) => {
    const target = d || date
    setLoading(true)
    try {
      const res = await api.get('/daily-report', {
        params: { date: target.format('YYYY-MM-DD') },
      })
      setReport(res.data)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '获取总结失败')
    } finally {
      setLoading(false)
    }
  }

  const buildText = (): string => {
    if (!report) return ''
    const lines: string[] = []
    for (const r of report.reports) {
      const total = r.counts.wo + r.counts.task + r.counts.log + r.counts.note
      if (total === 0) continue
      lines.push(`工作日志 ${report.date} (${total}条) ${r.displayName}`)
      if (r.workOrders.length > 0) {
        lines.push('')
        lines.push('工单')
        r.workOrders.forEach((w, i) => {
          lines.push(`${i + 1}、 ${w.orderNo} ${w.title} [${WO_STATUS[w.status]?.label || w.status}]`)
        })
      }
      if (r.tasks.length > 0) {
        lines.push('')
        lines.push('任务')
        r.tasks.forEach((t, i) => {
          lines.push(`${i + 1}、 ${t.taskNo} ${t.taskName} [${TASK_STATUS[t.status]?.label || t.status}]`)
        })
      }
      if (r.noteComments.length > 0) {
        lines.push('')
        lines.push('记事')
        r.noteComments.forEach((n, i) => {
          lines.push(`${i + 1}、 ${n.noteTitle}：${n.content}`)
        })
      }
      lines.push('')
      lines.push('明日计划')
      lines.push('重点跟进：')
      lines.push('')
    }
    return lines.join('\n')
  }

  const handleCopy = async () => {
    const text = buildText()
    if (!text) {
      message.warning('暂无内容可复制')
      return
    }
    try {
      await navigator.clipboard.writeText(text)
      message.success('已复制到剪贴板')
    } catch {
      const ta = document.createElement('textarea')
      ta.value = text
      document.body.appendChild(ta)
      ta.select()
      document.execCommand('copy')
      document.body.removeChild(ta)
      message.success('已复制到剪贴板')
    }
  }

  return (
    <div>
      <h2>总结</h2>

      <Space style={{ marginBottom: 16 }} wrap>
        <DatePicker
          value={date}
          onChange={(d) => {
            setDate(d || dayjs())
            fetchReport(d || dayjs())
          }}
        />
        <Button onClick={() => fetchReport(dayjs())}>今天</Button>
        <Button onClick={() => fetchReport(dayjs().subtract(1, 'day'))}>昨天</Button>
        <Button type="primary" icon={<ReloadOutlined />} onClick={() => fetchReport()}>
          刷新总结
        </Button>
        <Button icon={<CopyOutlined />} onClick={handleCopy}>
          复制文本总结
        </Button>
      </Space>

      <Spin spinning={loading}>
        {!report || report.reports.length === 0 ? (
          <Empty description="该日期暂无工作记录" style={{ marginTop: 80 }} />
        ) : (
          report.reports.map((r) => (
            <Card
              key={r.id}
              style={{ marginBottom: 16 }}
              title={
                <Space>
                  <span style={{ fontWeight: 700 }}>{r.displayName}</span>
                  <span style={{ fontSize: 12, color: '#999' }}>
                    工单 {r.counts.wo} | 任务 {r.counts.task} | 记事 {r.counts.note}
                  </span>
                </Space>
              }
            >
              {r.workOrders.length > 0 && (
                <div style={{ marginBottom: 8 }}>
                  <div style={{ fontWeight: 600, marginBottom: 4 }}>工单</div>
                  {r.workOrders.map((w, i) => (
                    <div key={i} style={{ padding: '4px 8px', background: '#fafafa', marginBottom: 4 }}>
                      <Tag color={WO_STATUS[w.status]?.color}>
                        {WO_STATUS[w.status]?.label || w.status}
                      </Tag>
                      <b>{w.orderNo}</b> {w.title}
                    </div>
                  ))}
                </div>
              )}
              {r.tasks.length > 0 && (
                <div style={{ marginBottom: 8 }}>
                  <div style={{ fontWeight: 600, marginBottom: 4 }}>任务</div>
                  {r.tasks.map((t, i) => (
                    <div key={i} style={{ padding: '4px 8px', background: '#fafafa', marginBottom: 4 }}>
                      <Tag color={TASK_STATUS[t.status]?.color}>
                        {TASK_STATUS[t.status]?.label || t.status}
                      </Tag>
                      <b>{t.taskNo}</b> {t.taskName}
                      {t.projectName && <span style={{ color: '#999' }}> [{t.projectName}]</span>}
                    </div>
                  ))}
                </div>
              )}
              {r.taskLogs.length > 0 && (
                <div style={{ marginBottom: 8 }}>
                  <div style={{ fontWeight: 600, marginBottom: 4 }}>反馈记录</div>
                  {r.taskLogs.slice(0, 5).map((l, i) => (
                    <div key={i} style={{ padding: '4px 8px', background: '#fafafa', marginBottom: 4 }}>
                      [{l.creatorName}] {l.content}
                    </div>
                  ))}
                </div>
              )}
              {r.noteComments.length > 0 && (
                <div>
                  <div style={{ fontWeight: 600, marginBottom: 4 }}>记事评论</div>
                  {r.noteComments.map((n, i) => (
                    <div key={i} style={{ padding: '4px 8px', background: '#f0f0ff', marginBottom: 4 }}>
                      <b>{n.noteTitle}</b>：{n.content}
                    </div>
                  ))}
                </div>
              )}
            </Card>
          ))
        )}
      </Spin>
    </div>
  )
}

export default DailyReport
