import React, { useState } from 'react'
import { Card, Typography, Table, Button, Upload, message, Space, Modal, Form, Input, Select } from 'antd'
import { UploadOutlined, DownloadOutlined, DeleteOutlined, PlusOutlined, EditOutlined } from '@ant-design/icons'
import axios from 'axios'
import './DataManagement.css'

const { Title } = Typography
const { Option } = Select

const DataManagement: React.FC = () => {
  const [data, setData] = useState<any[]>([])
  const [isModalVisible, setIsModalVisible] = useState(false)
  const [editingRecord, setEditingRecord] = useState<any>(null)
  const [form] = Form.useForm()

  // 模拟数据
  useState(() => {
    setData([
      { id: 1, name: '系统监控数据', type: '监控', size: '10.5 MB', createdAt: '2026-01-28 10:00', updatedAt: '2026-01-28 10:30' },
      { id: 2, name: '用户操作日志', type: '日志', size: '5.2 MB', createdAt: '2026-01-28 09:00', updatedAt: '2026-01-28 09:30' },
      { id: 3, name: '自动化任务记录', type: '任务', size: '2.8 MB', createdAt: '2026-01-28 08:00', updatedAt: '2026-01-28 08:30' },
      { id: 4, name: '系统配置', type: '配置', size: '0.5 MB', createdAt: '2026-01-27 16:00', updatedAt: '2026-01-27 16:30' },
    ])
  })

  const handleUpload = async (file: any) => {
    // 模拟文件上传
    message.success('文件上传成功')
    return false // 阻止自动上传
  }

  const handleDownload = (record: any) => {
    // 模拟文件下载
    message.success(`下载 ${record.name} 成功`)
  }

  const handleDelete = (record: any) => {
    // 模拟删除操作
    setData(data.filter(item => item.id !== record.id))
    message.success(`删除 ${record.name} 成功`)
  }

  const handleAdd = () => {
    setEditingRecord(null)
    form.resetFields()
    setIsModalVisible(true)
  }

  const handleEdit = (record: any) => {
    setEditingRecord(record)
    form.setFieldsValue(record)
    setIsModalVisible(true)
  }

  const handleSave = (values: any) => {
    if (editingRecord) {
      // 编辑现有记录
      setData(data.map(item => item.id === editingRecord.id ? { ...item, ...values } : item))
      message.success('编辑成功')
    } else {
      // 添加新记录
      const newRecord = {
        id: data.length + 1,
        ...values,
        createdAt: new Date().toISOString().slice(0, 19).replace('T', ' '),
        updatedAt: new Date().toISOString().slice(0, 19).replace('T', ' '),
      }
      setData([...data, newRecord])
      message.success('添加成功')
    }
    setIsModalVisible(false)
  }

  return (
    <div className="data-management-container">
      <Title level={2}>数据管理</Title>

      <Card className="data-upload-card">
        <Title level={4}>数据上传</Title>
        <Upload
          name="file"
          action="/api/data/upload"
          onChange={handleUpload}
          showUploadList={false}
        >
          <Button icon={<UploadOutlined />}>上传数据文件</Button>
        </Upload>
      </Card>

      <Card title="数据列表" className="data-list-card">
        <Space className="action-buttons">
          <Button type="primary" icon={<PlusOutlined />} onClick={handleAdd}>
            添加数据
          </Button>
        </Space>

        <Table dataSource={data} rowKey="id" className="data-table">
          <Table.Column title="名称" dataIndex="name" key="name" />
          <Table.Column title="类型" dataIndex="type" key="type" />
          <Table.Column title="大小" dataIndex="size" key="size" />
          <Table.Column title="创建时间" dataIndex="createdAt" key="createdAt" />
          <Table.Column title="更新时间" dataIndex="updatedAt" key="updatedAt" />
          <Table.Column
            title="操作"
            key="action"
            render={(text, record) => (
              <Space>
                <Button icon={<DownloadOutlined />} onClick={() => handleDownload(record)}>
                  下载
                </Button>
                <Button icon={<EditOutlined />} onClick={() => handleEdit(record)}>
                  编辑
                </Button>
                <Button danger icon={<DeleteOutlined />} onClick={() => handleDelete(record)}>
                  删除
                </Button>
              </Space>
            )}
          />
        </Table>
      </Card>

      <Modal
        title={editingRecord ? '编辑数据' : '添加数据'}
        open={isModalVisible}
        onCancel={() => setIsModalVisible(false)}
        footer={null}
      >
        <Form form={form} onFinish={handleSave}>
          <Form.Item name="name" label="名称" rules={[{ required: true, message: '请输入名称' }]}>
            <Input />
          </Form.Item>
          <Form.Item name="type" label="类型" rules={[{ required: true, message: '请选择类型' }]}>
            <Select>
              <Option value="监控">监控</Option>
              <Option value="日志">日志</Option>
              <Option value="任务">任务</Option>
              <Option value="配置">配置</Option>
            </Select>
          </Form.Item>
          <Form.Item name="size" label="大小" rules={[{ required: true, message: '请输入大小' }]}>
            <Input />
          </Form.Item>
          <Form.Item>
            <Space>
              <Button type="primary" htmlType="submit">
                保存
              </Button>
              <Button onClick={() => setIsModalVisible(false)}>
                取消
              </Button>
            </Space>
          </Form.Item>
        </Form>
      </Modal>
    </div>
  )
}

export default DataManagement