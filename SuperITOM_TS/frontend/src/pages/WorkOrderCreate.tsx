import React, { useState } from 'react'
import { Typography, Form, Input, Select, Button, Card, Space, message } from 'antd'
import { ArrowLeftOutlined } from '@ant-design/icons'
import { useNavigate } from 'react-router-dom'
import { api, PRIORITIES, CATEGORIES } from '../services/workOrder'

const { Title } = Typography
const { TextArea } = Input

const WorkOrderCreate: React.FC = () => {
  const navigate = useNavigate()
  const [form] = Form.useForm()
  const [submitting, setSubmitting] = useState(false)

  const handleSubmit = async (values: {
    title: string
    description?: string
    priority?: string
    category?: string
    subcategory?: string
    requestUser?: string
  }) => {
    setSubmitting(true)
    try {
      await api.post('/work-orders', values)
      message.success('工单创建成功')
      navigate('/work-orders')
    } catch (e: any) {
      message.error(e?.response?.data?.message || '创建失败')
    } finally {
      setSubmitting(false)
    }
  }

  return (
    <div>
      <Space style={{ marginBottom: 16 }}>
        <Button icon={<ArrowLeftOutlined />} onClick={() => navigate('/work-orders')}>
          返回列表
        </Button>
      </Space>

      <Title level={2}>创建工单</Title>

      <Card style={{ maxWidth: 720 }}>
        <Form
          form={form}
          layout="vertical"
          initialValues={{ priority: '中', category: '一般' }}
          onFinish={handleSubmit}
        >
          <Form.Item
            name="title"
            label="标题"
            rules={[{ required: true, message: '请输入工单标题' }]}
          >
            <Input placeholder="请输入工单标题" />
          </Form.Item>

          <Form.Item name="description" label="描述">
            <TextArea rows={5} placeholder="请输入问题/需求描述" />
          </Form.Item>

          <Space size="large" wrap>
            <Form.Item name="priority" label="优先级">
              <Select style={{ width: 160 }} options={PRIORITIES.map((p) => ({ value: p, label: p }))} />
            </Form.Item>

            <Form.Item name="category" label="分类">
              <Select style={{ width: 160 }} options={CATEGORIES.map((c) => ({ value: c, label: c }))} />
            </Form.Item>
          </Space>

          <Form.Item name="subcategory" label="子分类">
            <Input placeholder="可选" />
          </Form.Item>

          <Form.Item name="requestUser" label="请求用户">
            <Input placeholder="可选，如：谢芳材-供应链中心" />
          </Form.Item>

          <Form.Item>
            <Space>
              <Button type="primary" htmlType="submit" loading={submitting}>
                提交工单
              </Button>
              <Button onClick={() => navigate('/work-orders')}>取消</Button>
            </Space>
          </Form.Item>
        </Form>
      </Card>
    </div>
  )
}

export default WorkOrderCreate
