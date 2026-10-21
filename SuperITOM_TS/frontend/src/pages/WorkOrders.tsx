import React, { useState, useEffect } from 'react'
import { Typography, Table, Tag, Button, Space, Radio, message } from 'antd'
import { PlusOutlined, ReloadOutlined } from '@ant-design/icons'
import { useNavigate } from 'react-router-dom'
import {
  api,
  WorkOrderListItem,
  STATUS_META,
  PRIORITY_COLOR,
  STATUS_FILTERS,
} from '../services/workOrder'

const { Title } = Typography

const WorkOrders: React.FC = () => {
  const navigate = useNavigate()
  const [data, setData] = useState<WorkOrderListItem[]>([])
  const [status, setStatus] = useState('all')
  const [loading, setLoading] = useState(false)

  const fetchList = async (filter?: string) => {
    setLoading(true)
    try {
      const res = await api.get('/work-orders', {
        params: { status: filter ?? status },
      })
      setData(res.data)
    } catch (e: any) {
      message.error(e?.response?.data?.message || '获取工单列表失败')
    } finally {
      setLoading(false)
    }
  }

  useEffect(() => {
    fetchList()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  const handleStatusChange = (value: string) => {
    setStatus(value)
    fetchList(value)
  }

  return (
    <div>
      <Title level={2}>工单</Title>

      <Space style={{ marginBottom: 16 }} wrap>
        <Radio.Group
          value={status}
          onChange={(e) => handleStatusChange(e.target.value)}
          optionType="button"
          options={STATUS_FILTERS}
        />
        <Button icon={<ReloadOutlined />} onClick={() => fetchList()}>
          刷新
        </Button>
        <Button type="primary" icon={<PlusOutlined />} onClick={() => navigate('/work-orders/create')}>
          新建工单
        </Button>
      </Space>

      <Table
        dataSource={data}
        rowKey="id"
        loading={loading}
        pagination={{ pageSize: 15 }}
        onRow={(record) => ({
          onClick: () => navigate(`/work-orders/${record.id}`),
          style: { cursor: 'pointer' },
        })}
      >
        <Table.Column title="工单号" dataIndex="orderNo" key="orderNo" width={170} />
        <Table.Column title="标题" dataIndex="title" key="title" ellipsis />
        <Table.Column
          title="优先级"
          dataIndex="priority"
          key="priority"
          width={90}
          render={(p: string) => <Tag color={PRIORITY_COLOR[p] || 'gray'}>{p}</Tag>}
        />
        <Table.Column
          title="状态"
          dataIndex="status"
          key="status"
          width={90}
          render={(s: string) => (
            <Tag color={STATUS_META[s]?.color}>{STATUS_META[s]?.label || s}</Tag>
          )}
        />
        <Table.Column title="分类" dataIndex="category" key="category" width={100} />
        <Table.Column title="创建人" dataIndex="creatorName" key="creatorName" width={110} />
        <Table.Column
          title="指派人"
          dataIndex="assigneeName"
          key="assigneeName"
          width={110}
          render={(v: string) => v || '-'}
        />
        <Table.Column title="创建时间" dataIndex="createdAt" key="createdAt" width={170} />
      </Table>
    </div>
  )
}

export default WorkOrders
