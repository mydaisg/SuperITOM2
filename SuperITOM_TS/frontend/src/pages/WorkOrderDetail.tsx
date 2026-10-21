import React, { useState, useEffect, useCallback } from 'react'
import {
  Typography,
  Tag,
  Button,
  Space,
  Card,
  Descriptions,
  Input,
  Select,
  List,
  Divider,
  message,
  Spin,
} from 'antd'
import { ArrowLeftOutlined, SendOutlined } from '@ant-design/icons'
import { useParams, useNavigate } from 'react-router-dom'
import {
  api,
  WorkOrderDetail as Detail,
  WorkOrderComment,
  AssignableUser,
  STATUS_META,
  PRIORITY_COLOR,
  currentUser,
  canAssign,
} from '../services/workOrder'

const { Title, Text } = Typography
const { TextArea } = Input

const WorkOrderDetail: React.FC = () => {
  const { id } = useParams<{ id: string }>()
  const navigate = useNavigate()
  const workOrderId = Number(id)

  const [detail, setDetail] = useState<Detail | null>(null)
  const [comments, setComments] = useState<WorkOrderComment[]>([])
  const [assignableUsers, setAssignableUsers] = useState<AssignableUser[]>([])
  const [loading, setLoading] = useState(true)

  const [assigneeId, setAssigneeId] = useState<number>()
  const [resolution, setResolution] = useState('')
  const [closeReason, setCloseReason] = useState('')
  const [commentInput, setCommentInput] = useState('')
  const [submitting, setSubmitting] = useState(false)

  const me = currentUser()

  const fetchDetail = useCallback(async () => {
    setLoading(true)
    try {
      const [detailRes, commentRes] = await Promise.all([
        api.get(`/work-orders/${workOrderId}`),
        api.get(`/work-orders/${workOrderId}/comments`),
      ])
      setDetail(detailRes.data)
      setComments(commentRes.data)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '获取工单详情失败')
    } finally {
      setLoading(false)
    }
  }, [workOrderId])

  const fetchAssignableUsers = useCallback(async () => {
    try {
      const res = await api.get('/work-orders/assignable-users')
      setAssignableUsers(res.data)
    } catch {
      // 忽略：无派发权限时不展示下拉
    }
  }, [])

  useEffect(() => {
    fetchDetail()
    if (canAssign()) {
      fetchAssignableUsers()
    }
  }, [fetchDetail, fetchAssignableUsers])

  const doAction = async (fn: () => Promise<unknown>, successMsg: string) => {
    setSubmitting(true)
    try {
      await fn()
      message.success(successMsg)
      setResolution('')
      setCloseReason('')
      await fetchDetail()
    } catch (e: any) {
      message.error(e?.response?.data?.message || '操作失败')
    } finally {
      setSubmitting(false)
    }
  }

  const handleAssign = () =>
    doAction(
      () => api.post(`/work-orders/${workOrderId}/assign`, { assigneeId }),
      '派发成功',
    )

  const handleStart = () =>
    doAction(() => api.post(`/work-orders/${workOrderId}/start`), '已开始处理')

  const handleComplete = () =>
    doAction(
      () => api.post(`/work-orders/${workOrderId}/complete`, { resolution }),
      '工单已完成',
    )

  const handleClose = () =>
    doAction(
      () => api.post(`/work-orders/${workOrderId}/close`, { reason: closeReason }),
      '工单已关闭',
    )

  const handleAddComment = async () => {
    const content = commentInput.trim()
    if (!content) return
    setSubmitting(true)
    try {
      const res = await api.post(`/work-orders/${workOrderId}/comments`, {
        comment: content,
      })
      setComments(res.data)
      setCommentInput('')
    } catch (e: any) {
      message.error(e?.response?.data?.message || '评论失败')
    } finally {
      setSubmitting(false)
    }
  }

  if (loading && !detail) {
    return (
      <div style={{ textAlign: 'center', padding: 80 }}>
        <Spin size="large" />
      </div>
    )
  }

  if (!detail) {
    return (
      <div>
        <Button icon={<ArrowLeftOutlined />} onClick={() => navigate('/work-orders')}>
          返回列表
        </Button>
        <Divider />
        <Text type="secondary">工单不存在</Text>
      </div>
    )
  }

  const isTerminal = detail.status === 'completed' || detail.status === 'closed'
  const statusMeta = STATUS_META[detail.status] || { label: detail.status, color: 'gray' }

  const canStart = detail.status === 'assigned' && detail.assigneeId === me.id
  const canComplete = detail.status === 'processing' && detail.handlerId === me.id

  return (
    <div>
      <Space style={{ marginBottom: 16 }}>
        <Button icon={<ArrowLeftOutlined />} onClick={() => navigate('/work-orders')}>
          返回列表
        </Button>
      </Space>

      <Title level={3} style={{ marginBottom: 8 }}>
        {detail.title}
      </Title>
      <Space size="middle" style={{ marginBottom: 16 }}>
        <Text type="secondary">{detail.orderNo}</Text>
        <Tag color={statusMeta.color}>{statusMeta.label}</Tag>
        <Tag color={PRIORITY_COLOR[detail.priority] || 'gray'}>{detail.priority}</Tag>
      </Space>

      <Card title="工单信息" style={{ marginBottom: 16 }}>
        <Descriptions column={2} bordered size="small">
          <Descriptions.Item label="创建人">{detail.creatorName || '-'}</Descriptions.Item>
          <Descriptions.Item label="指派给">{detail.assigneeName || '未分配'}</Descriptions.Item>
          {detail.handlerName && (
            <Descriptions.Item label="处理人">{detail.handlerName}</Descriptions.Item>
          )}
          {detail.requestUser && (
            <Descriptions.Item label="请求用户">{detail.requestUser}</Descriptions.Item>
          )}
          <Descriptions.Item label="分类">
            {detail.subcategory
              ? `${detail.category} / ${detail.subcategory}`
              : detail.category}
          </Descriptions.Item>
          <Descriptions.Item label="创建时间">{detail.createdAt || '-'}</Descriptions.Item>
          {detail.assignedAt && (
            <Descriptions.Item label="派发时间">{detail.assignedAt}</Descriptions.Item>
          )}
          {detail.completedAt && (
            <Descriptions.Item label="完成时间">{detail.completedAt}</Descriptions.Item>
          )}
          {detail.resolution && (
            <Descriptions.Item label="解决方案" span={2}>
              {detail.resolution}
            </Descriptions.Item>
          )}
          {detail.description && (
            <Descriptions.Item label="描述" span={2}>
              {detail.description}
            </Descriptions.Item>
          )}
        </Descriptions>
      </Card>

      {!isTerminal && (
        <Card title="操作" style={{ marginBottom: 16 }}>
          {detail.status === 'pending' && canAssign() && (
            <Space wrap style={{ marginBottom: 12 }}>
              <span>派发给</span>
              <Select
                style={{ width: 220 }}
                placeholder="选择用户"
                value={assigneeId}
                onChange={(v: number) => setAssigneeId(v)}
                options={assignableUsers.map((u) => ({
                  value: u.id,
                  label: `${u.displayName} (${u.role})`,
                }))}
              />
              <Button
                type="primary"
                disabled={!assigneeId}
                loading={submitting}
                onClick={handleAssign}
              >
                确认派发
              </Button>
            </Space>
          )}

          {canStart && (
            <Button type="primary" loading={submitting} onClick={handleStart}>
              开始处理
            </Button>
          )}

          {canComplete && (
            <Space wrap>
              <span>解决方案</span>
              <Input
                style={{ width: 320 }}
                value={resolution}
                onChange={(e) => setResolution(e.target.value)}
                placeholder="请输入解决方案"
              />
              <Button type="primary" loading={submitting} onClick={handleComplete}>
                完成
              </Button>
            </Space>
          )}

          <Divider />

          <Space wrap>
            <span>关闭原因</span>
            <Input
              style={{ width: 320 }}
              value={closeReason}
              onChange={(e) => setCloseReason(e.target.value)}
              placeholder="可选"
            />
            <Button danger loading={submitting} onClick={handleClose}>
              关闭工单
            </Button>
          </Space>
        </Card>
      )}

      <Card title={`评论 (${comments.length})`} style={{ marginBottom: 16 }}>
        <List
          dataSource={comments}
          locale={{ emptyText: '暂无评论' }}
          renderItem={(c) => (
            <List.Item key={c.id}>
              <List.Item.Meta
                title={
                  <Space>
                    <Text strong>{c.creatorName || '未知'}</Text>
                    <Text type="secondary" style={{ fontSize: 12 }}>
                      {c.createdAt}
                    </Text>
                  </Space>
                }
                description={<div style={{ whiteSpace: 'pre-wrap' }}>{c.comment}</div>}
              />
            </List.Item>
          )}
        />
        <Divider />
        <Space.Compact style={{ width: '100%' }}>
          <TextArea
            value={commentInput}
            onChange={(e) => setCommentInput(e.target.value)}
            placeholder="输入评论…"
            autoSize={{ minRows: 2, maxRows: 4 }}
          />
          <Button
            type="primary"
            icon={<SendOutlined />}
            loading={submitting}
            onClick={handleAddComment}
          >
            发送
          </Button>
        </Space.Compact>
      </Card>
    </div>
  )
}

export default WorkOrderDetail
