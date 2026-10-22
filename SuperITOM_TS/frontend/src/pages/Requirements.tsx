import React, { useState, useEffect } from 'react'
import {
  Table,
  Tag,
  Button,
  Space,
  Input,
  message,
  Modal,
  Drawer,
  List,
  Select,
  Form,
  Popconfirm,
} from 'antd'
import { PlusOutlined, ReloadOutlined } from '@ant-design/icons'
import { api } from '../services/workOrder'

interface Requirement {
  id: number
  reqNo: string
  title: string
  description: string
  status: string
  priority: string
  owner: string
  startDate: string
  dueDate: string
  createdAt: string
  updatedAt: string
}

interface Progress {
  id: number
  requirementId: number
  dept: string
  person: string
  status: string
  progressDate: string
  content: string
}

const STATUSES = ['待开始', '进行中', '已完成', '已暂停', '已取消']
const PRIORITIES = ['低', '中', '高', '紧急']

const STATUS_COLOR: Record<string, string> = {
  待开始: 'gray',
  进行中: 'blue',
  已完成: 'green',
  已暂停: 'orange',
  已取消: 'red',
}

const PRIORITY_COLOR: Record<string, string> = {
  低: 'gray',
  中: 'blue',
  高: 'orange',
  紧急: 'red',
}

const Requirements: React.FC = () => {
  const [data, setData] = useState<Requirement[]>([])
  const [loading, setLoading] = useState(false)
  const [status, setStatus] = useState<string>()
  const [keyword, setKeyword] = useState('')

  const [detail, setDetail] = useState<Requirement | null>(null)
  const [drawerOpen, setDrawerOpen] = useState(false)
  const [progress, setProgress] = useState<Progress[]>([])

  const [modalOpen, setModalOpen] = useState(false)
  const [editing, setEditing] = useState<Requirement | null>(null)
  const [form] = Form.useForm()

  const [progressInput, setProgressInput] = useState('')

  const fetchList = async () => {
    setLoading(true)
    try {
      const res = await api.get('/requirements', { params: { status, keyword } })
      setData(res.data)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '获取需求列表失败')
    } finally {
      setLoading(false)
    }
  }

  useEffect(() => {
    fetchList()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  const openDetail = async (r: Requirement) => {
    setDetail(r)
    setDrawerOpen(true)
    try {
      const res = await api.get(`/requirements/${r.id}/progress`)
      setProgress(res.data)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '获取进度失败')
    }
  }

  const openCreate = () => {
    setEditing(null)
    form.resetFields()
    form.setFieldsValue({ status: '进行中', priority: '中' })
    setModalOpen(true)
  }

  const openEdit = (r: Requirement) => {
    setEditing(r)
    form.setFieldsValue(r)
    setModalOpen(true)
  }

  const handleSave = async () => {
    const values = await form.validateFields()
    try {
      if (editing) {
        await api.put(`/requirements/${editing.id}`, values)
        message.success('已更新')
      } else {
        await api.post('/requirements', values)
        message.success('已创建')
      }
      setModalOpen(false)
      fetchList()
    } catch (e: any) {
      message.error(e?.response?.data?.message || '保存失败')
    }
  }

  const handleDelete = async (id: number) => {
    try {
      await api.delete(`/requirements/${id}`)
      message.success('已删除')
      setDrawerOpen(false)
      fetchList()
    } catch (e: any) {
      message.error(e?.response?.data?.message || '删除失败')
    }
  }

  const addProgress = async () => {
    if (!detail || !progressInput.trim()) return
    try {
      const res = await api.post(`/requirements/${detail.id}/progress`, {
        content: progressInput,
      })
      setProgress(res.data)
      setProgressInput('')
    } catch (e: any) {
      message.error(e?.response?.data?.message || '添加进度失败')
    }
  }

  const deleteProgress = async (id: number) => {
    try {
      await api.delete(`/requirements/progress/${id}`)
      if (detail) {
        const res = await api.get(`/requirements/${detail.id}/progress`)
        setProgress(res.data)
      }
    } catch (e: any) {
      message.error(e?.response?.data?.message || '删除失败')
    }
  }

  return (
    <div>
      <h2>需求</h2>

      <Space style={{ marginBottom: 16 }} wrap>
        <Select
          placeholder="全部状态"
          allowClear
          style={{ width: 140 }}
          value={status}
          onChange={(v) => setStatus(v)}
          options={STATUSES.map((s) => ({ value: s, label: s }))}
        />
        <Input.Search
          placeholder="搜索需求…"
          allowClear
          style={{ width: 220 }}
          onSearch={() => fetchList()}
          onChange={(e) => setKeyword(e.target.value)}
        />
        <Button onClick={fetchList}>查询</Button>
        <Button icon={<ReloadOutlined />} onClick={fetchList}>
          刷新
        </Button>
        <Button type="primary" icon={<PlusOutlined />} onClick={openCreate}>
          新建需求
        </Button>
      </Space>

      <Table
        dataSource={data}
        rowKey="id"
        loading={loading}
        pagination={{ pageSize: 15 }}
        onRow={(record) => ({
          onClick: () => openDetail(record),
          style: { cursor: 'pointer' },
        })}
      >
        <Table.Column title="编号" dataIndex="reqNo" key="reqNo" width={150} />
        <Table.Column title="标题" dataIndex="title" key="title" ellipsis />
        <Table.Column
          title="状态"
          dataIndex="status"
          key="status"
          width={90}
          render={(s: string) => <Tag color={STATUS_COLOR[s] || 'gray'}>{s}</Tag>}
        />
        <Table.Column
          title="优先级"
          dataIndex="priority"
          key="priority"
          width={90}
          render={(p: string) => <Tag color={PRIORITY_COLOR[p] || 'gray'}>{p}</Tag>}
        />
        <Table.Column title="负责人" dataIndex="owner" key="owner" width={110} />
        <Table.Column title="开始日期" dataIndex="startDate" key="startDate" width={110} />
        <Table.Column title="截止日期" dataIndex="dueDate" key="dueDate" width={110} />
        <Table.Column
          title="操作"
          key="action"
          width={80}
          render={(_: any, r: Requirement) => (
            <Button
              type="link"
              size="small"
              onClick={(e) => {
                e.stopPropagation()
                openEdit(r)
              }}
            >
              编辑
            </Button>
          )}
        />
      </Table>

      <Modal
        title={editing ? '编辑需求' : '新建需求'}
        open={modalOpen}
        onOk={handleSave}
        onCancel={() => setModalOpen(false)}
        okText="保存"
        cancelText="取消"
      >
        <Form form={form} layout="vertical">
          <Form.Item name="title" label="标题" rules={[{ required: true, message: '请输入标题' }]}>
            <Input />
          </Form.Item>
          <Form.Item name="description" label="描述">
            <Input.TextArea rows={3} />
          </Form.Item>
          <Space>
            <Form.Item name="status" label="状态">
              <Select style={{ width: 120 }} options={STATUSES.map((s) => ({ value: s, label: s }))} />
            </Form.Item>
            <Form.Item name="priority" label="优先级">
              <Select style={{ width: 120 }} options={PRIORITIES.map((s) => ({ value: s, label: s }))} />
            </Form.Item>
            <Form.Item name="owner" label="负责人">
              <Input style={{ width: 140 }} />
            </Form.Item>
          </Space>
          <Space>
            <Form.Item name="startDate" label="开始日期">
              <Input placeholder="YYYY-MM-DD" />
            </Form.Item>
            <Form.Item name="dueDate" label="截止日期">
              <Input placeholder="YYYY-MM-DD" />
            </Form.Item>
          </Space>
        </Form>
      </Modal>

      <Drawer
        title={detail?.title}
        width={560}
        open={drawerOpen}
        onClose={() => setDrawerOpen(false)}
        extra={
          detail && (
            <Space>
              <Button onClick={() => openEdit(detail)}>编辑</Button>
              <Popconfirm title="确定删除该需求？" onConfirm={() => handleDelete(detail.id)}>
                <Button danger>删除</Button>
              </Popconfirm>
            </Space>
          )
        }
      >
        {detail && (
          <>
            <Space style={{ marginBottom: 12 }} wrap>
              <span style={{ color: '#999' }}>{detail.reqNo}</span>
              <Tag color={STATUS_COLOR[detail.status] || 'gray'}>{detail.status}</Tag>
              <Tag color={PRIORITY_COLOR[detail.priority] || 'gray'}>{detail.priority}</Tag>
              {detail.owner && <span>负责人：{detail.owner}</span>}
              {detail.startDate && <span style={{ color: '#999' }}>{detail.startDate}</span>}
              {detail.dueDate && <span style={{ color: '#999' }}>~ {detail.dueDate}</span>}
            </Space>
            <div style={{ whiteSpace: 'pre-wrap', marginBottom: 16, color: '#555' }}>
              {detail.description || '（无描述）'}
            </div>

            <h4>进度记录 ({progress.length})</h4>
            <List
              size="small"
              dataSource={progress}
              locale={{ emptyText: '暂无进度' }}
              renderItem={(p) => (
                <List.Item
                  actions={[
                    <Button
                      size="small"
                      type="link"
                      danger
                      key="del"
                      onClick={() => deleteProgress(p.id)}
                    >
                      删除
                    </Button>,
                  ]}
                >
                  <List.Item.Meta
                    title={
                      <Space size={4}>
                        {p.status && <Tag color={STATUS_COLOR[p.status] || 'blue'}>{p.status}</Tag>}
                        {p.dept && <span style={{ fontSize: 12, color: '#999' }}>{p.dept}</span>}
                        {p.person && <span style={{ fontSize: 12, color: '#999' }}>{p.person}</span>}
                        <span style={{ fontSize: 12, color: '#999' }}>{p.progressDate}</span>
                      </Space>
                    }
                    description={<div style={{ whiteSpace: 'pre-wrap' }}>{p.content}</div>}
                  />
                </List.Item>
              )}
            />

            <Space.Compact style={{ width: '100%', marginTop: 12 }} block>
              <Input
                placeholder="进度内容…"
                value={progressInput}
                onChange={(e) => setProgressInput(e.target.value)}
                onPressEnter={addProgress}
              />
              <Button type="primary" onClick={addProgress}>
                添加
              </Button>
            </Space.Compact>
          </>
        )}
      </Drawer>
    </div>
  )
}

export default Requirements
