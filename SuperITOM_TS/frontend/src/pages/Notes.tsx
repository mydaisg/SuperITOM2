import React, { useState, useEffect } from 'react'
import {
  Row,
  Col,
  Card,
  List,
  Tag,
  Button,
  Input,
  Space,
  Radio,
  message,
  Popconfirm,
  Modal,
  Empty,
  Tooltip,
} from 'antd'
import { PlusOutlined, PushpinOutlined, DeleteOutlined } from '@ant-design/icons'
import { api } from '../services/workOrder'

interface Note {
  id: number
  noteNo: string
  title: string
  content: string
  status: string
  importance: number
  pinned: number
  creatorName: string
  createdAt: string
  updatedAt: string
  commentCount: number
}

interface NoteComment {
  id: number
  content: string
  creatorName: string
  createdAt: string
  status: string
}

const NOTE_STATUS: Record<string, { label: string; color: string }> = {
  pending: { label: '待处理', color: 'orange' },
  active: { label: '进行中', color: 'blue' },
  completed: { label: '已完成', color: 'green' },
  suspended: { label: '已挂起', color: 'red' },
}

const STATUS_FILTERS = [
  { value: 'all', label: '全部' },
  { value: 'pending', label: '待处理' },
  { value: 'active', label: '进行中' },
  { value: 'completed', label: '已完成' },
  { value: 'suspended', label: '已挂起' },
]

const Notes: React.FC = () => {
  const [notes, setNotes] = useState<Note[]>([])
  const [selected, setSelected] = useState<Note | null>(null)
  const [comments, setComments] = useState<NoteComment[]>([])
  const [statusFilter, setStatusFilter] = useState('all')
  const [newText, setNewText] = useState('')
  const [commentText, setCommentText] = useState('')
  const [editOpen, setEditOpen] = useState(false)
  const [editTitle, setEditTitle] = useState('')
  const [editContent, setEditContent] = useState('')
  const [loading, setLoading] = useState(false)

  const fetchList = async (id?: number) => {
    setLoading(true)
    try {
      const res = await api.get('/notes')
      setNotes(res.data)
      if (id) {
        const cur = res.data.find((n: Note) => n.id === id)
        if (cur) setSelected(cur)
      }
    } catch (e: any) {
      message.error(e?.response?.data?.message || '获取记事列表失败')
    } finally {
      setLoading(false)
    }
  }

  const fetchComments = async (id: number) => {
    try {
      const res = await api.get(`/notes/${id}/comments`)
      setComments(res.data)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '获取评论失败')
    }
  }

  const selectNote = async (n: Note) => {
    try {
      const res = await api.get(`/notes/${n.id}`)
      setSelected(res.data)
      fetchComments(n.id)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '获取记事详情失败')
    }
  }

  useEffect(() => {
    fetchList()
  }, [])

  const filtered = notes.filter((n) => statusFilter === 'all' || n.status === statusFilter)

  const handleCreate = async () => {
    if (!newText.trim()) {
      message.warning('内容不能为空（第一行作为标题）')
      return
    }
    try {
      const res = await api.post('/notes', { text: newText })
      setNewText('')
      message.success('已创建')
      fetchList(res.data.id)
      selectNote(res.data)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '创建失败')
    }
  }

  const handleDelete = async () => {
    if (!selected) return
    try {
      await api.delete(`/notes/${selected.id}`)
      message.success('已删除')
      setSelected(null)
      setComments([])
      fetchList()
    } catch (e: any) {
      message.error(e?.response?.data?.message || '删除失败')
    }
  }

  const handlePatch = async (action: 'status' | 'importance' | 'pin', value?: any) => {
    if (!selected) return
    try {
      let res
      if (action === 'status') {
        res = await api.patch(`/notes/${selected.id}/status`, { status: value })
      } else if (action === 'importance') {
        res = await api.patch(`/notes/${selected.id}/importance`, {
          importance: Math.max(0, Math.min(5, selected.importance + value)),
        })
      } else {
        res = await api.patch(`/notes/${selected.id}/pin`)
      }
      setSelected(res.data)
      fetchList(res.data.id)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '操作失败')
    }
  }

  const handleEditSave = async () => {
    if (!selected) return
    try {
      const res = await api.put(`/notes/${selected.id}`, {
        title: editTitle,
        content: editContent,
      })
      setEditOpen(false)
      setSelected(res.data)
      fetchList(res.data.id)
      message.success('已保存')
    } catch (e: any) {
      message.error(e?.response?.data?.message || '保存失败')
    }
  }

  const handleAddComment = async () => {
    if (!selected || !commentText.trim()) return
    try {
      const res = await api.post(`/notes/${selected.id}/comments`, { content: commentText })
      setCommentText('')
      setComments(res.data)
      fetchList(selected.id)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '评论失败')
    }
  }

  const handleMarkComment = async (c: NoteComment) => {
    try {
      await api.patch(`/notes/comments/${c.id}/mark`, { completed: c.status !== 'completed' })
      if (selected) fetchComments(selected.id)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '操作失败')
    }
  }

  const handleDeleteComment = async (c: NoteComment) => {
    try {
      await api.delete(`/notes/comments/${c.id}`)
      if (selected) {
        fetchComments(selected.id)
        fetchList(selected.id)
      }
    } catch (e: any) {
      message.error(e?.response?.data?.message || '删除失败')
    }
  }

  return (
    <div>
      <h2>记事</h2>

      <Row gutter={16}>
        <Col span={9}>
          <Card size="small" title="新建记事" style={{ marginBottom: 12 }}>
            <Input.TextArea
              rows={2}
              placeholder="第一行作为标题…"
              value={newText}
              onChange={(e) => setNewText(e.target.value)}
            />
            <Button
              type="primary"
              icon={<PlusOutlined />}
              style={{ marginTop: 8 }}
              onClick={handleCreate}
            >
              添加记事
            </Button>
          </Card>

          <Radio.Group
            value={statusFilter}
            onChange={(e) => setStatusFilter(e.target.value)}
            optionType="button"
            options={STATUS_FILTERS}
            style={{ marginBottom: 12 }}
          />

          <List
            loading={loading}
            dataSource={filtered}
            locale={{ emptyText: <Empty description="暂无记事" /> }}
            renderItem={(n) => (
              <List.Item
                onClick={() => selectNote(n)}
                style={{
                  cursor: 'pointer',
                  background: selected?.id === n.id ? '#e6f7ff' : undefined,
                  padding: 8,
                  borderRadius: 6,
                }}
              >
                <List.Item.Meta
                  title={
                    <Space size={4}>
                      {n.pinned > 0 && <PushpinOutlined style={{ color: '#fa8c16' }} />}
                      {'🚩'.repeat(n.importance)}
                      <span>{n.title}</span>
                    </Space>
                  }
                  description={
                    <Space size={4} wrap>
                      <Tag color={NOTE_STATUS[n.status]?.color}>
                        {NOTE_STATUS[n.status]?.label || n.status}
                      </Tag>
                      <span style={{ fontSize: 12, color: '#999' }}>
                        {n.commentCount} 评论
                      </span>
                    </Space>
                  }
                />
              </List.Item>
            )}
          />
        </Col>

        <Col span={15}>
          {!selected ? (
            <Empty description="选择左侧记事查看详情" style={{ marginTop: 80 }} />
          ) : (
            <Card
              title={
                <Space>
                  {selected.pinned > 0 && <PushpinOutlined style={{ color: '#fa8c16' }} />}
                  <span>{selected.title}</span>
                  <Tag color={NOTE_STATUS[selected.status]?.color}>
                    {NOTE_STATUS[selected.status]?.label || selected.status}
                  </Tag>
                </Space>
              }
              extra={
                <Space>
                  <Tooltip title="置顶">
                    <Button
                      icon={<PushpinOutlined />}
                      type={selected.pinned > 0 ? 'primary' : 'default'}
                      onClick={() => handlePatch('pin')}
                    />
                  </Tooltip>
                  <Button onClick={() => handlePatch('importance', -1)}>🚩-</Button>
                  <Button onClick={() => handlePatch('importance', 1)}>🚩+</Button>
                  <Button
                    onClick={() => {
                      setEditTitle(selected.title)
                      setEditContent(selected.content)
                      setEditOpen(true)
                    }}
                  >
                    编辑
                  </Button>
                  <Popconfirm title="确定删除该记事？" onConfirm={handleDelete}>
                    <Button danger icon={<DeleteOutlined />}>
                      删除
                    </Button>
                  </Popconfirm>
                </Space>
              }
            >
              <div style={{ marginBottom: 12 }}>
                <Space>
                  <span style={{ color: '#999' }}>{selected.noteNo}</span>
                  <span style={{ color: '#999' }}>{selected.creatorName}</span>
                  <span style={{ color: '#999' }}>{selected.createdAt}</span>
                </Space>
                <div style={{ margin: '8px 0' }}>
                  <Radio.Group
                    size="small"
                    value={selected.status}
                    onChange={(e) => handlePatch('status', e.target.value)}
                    optionType="button"
                    options={[
                      { value: 'pending', label: '待处理' },
                      { value: 'active', label: '进行中' },
                      { value: 'completed', label: '已完成' },
                      { value: 'suspended', label: '挂起' },
                    ]}
                  />
                </div>
              </div>

              <div style={{ whiteSpace: 'pre-wrap', minHeight: 80, marginBottom: 16 }}>
                {selected.content || '（无正文）'}
              </div>

              <h4>评论 ({comments.length})</h4>
              <List
                size="small"
                dataSource={comments}
                locale={{ emptyText: '暂无评论' }}
                renderItem={(c) => (
                  <List.Item
                    actions={[
                      <Button
                        size="small"
                        type="link"
                        key="mark"
                        onClick={() => handleMarkComment(c)}
                      >
                        {c.status === 'completed' ? '取消完成' : '完成'}
                      </Button>,
                      <Button
                        size="small"
                        type="link"
                        danger
                        key="del"
                        onClick={() => handleDeleteComment(c)}
                      >
                        删除
                      </Button>,
                    ]}
                  >
                    <List.Item.Meta
                      title={
                        <Space size={4}>
                          <span style={{ fontWeight: 600 }}>
                            {c.creatorName}
                            {c.status === 'completed' && ' ✅'}
                          </span>
                          <span style={{ fontSize: 12, color: '#999' }}>{c.createdAt}</span>
                        </Space>
                      }
                      description={<div style={{ whiteSpace: 'pre-wrap' }}>{c.content}</div>}
                    />
                  </List.Item>
                )}
              />

              <Space.Compact style={{ width: '100%', marginTop: 12 }}>
                <Input
                  placeholder="输入评论…"
                  value={commentText}
                  onChange={(e) => setCommentText(e.target.value)}
                  onPressEnter={handleAddComment}
                />
                <Button type="primary" onClick={handleAddComment}>
                  发送
                </Button>
              </Space.Compact>
            </Card>
          )}
        </Col>
      </Row>

      <Modal
        title="编辑记事"
        open={editOpen}
        onOk={handleEditSave}
        onCancel={() => setEditOpen(false)}
        okText="保存"
        cancelText="取消"
      >
        <Input
          placeholder="标题"
          value={editTitle}
          onChange={(e) => setEditTitle(e.target.value)}
          style={{ marginBottom: 8 }}
        />
        <Input.TextArea
          rows={6}
          placeholder="正文"
          value={editContent}
          onChange={(e) => setEditContent(e.target.value)}
        />
      </Modal>
    </div>
  )
}

export default Notes
