import React, { useState } from 'react'
import { Card, Typography, Form, Input, Select, Switch, Button, message, Space } from 'antd'
import { SaveOutlined, ReloadOutlined } from '@ant-design/icons'
import axios from 'axios'
import './Settings.css'

const { Title } = Typography
const { Option } = Select

const Settings: React.FC = () => {
  const [form] = Form.useForm()
  const [isLoading, setIsLoading] = useState(false)

  // 模拟系统设置数据
  useState(() => {
    form.setFieldsValue({
      appName: 'SuperITOM_TS',
      appVersion: '1.0.0',
      apiEndpoint: 'http://localhost:3000',
      databaseHost: 'localhost',
      databasePort: '5432',
      databaseName: 'superitom',
      databaseUsername: 'admin',
      enableLogging: true,
      enableMonitoring: true,
      enableGithubIntegration: true,
      logLevel: 'info',
      maxConcurrentTasks: '10',
    })
  })

  const handleSave = async (values: any) => {
    setIsLoading(true)
    try {
      // 模拟保存设置
      setTimeout(() => {
        message.success('设置保存成功')
        setIsLoading(false)
      }, 1000)
    } catch (error) {
      message.error('设置保存失败')
      setIsLoading(false)
    }
  }

  const handleReset = () => {
    // 重置表单
    form.resetFields()
    message.success('设置已重置')
  }

  return (
    <div className="settings-container">
      <Title level={2}>系统设置</Title>

      <Card className="settings-card">
        <Form form={form} onFinish={handleSave} className="settings-form">
          <Card title="基本设置">
            <Form.Item name="appName" label="应用名称" rules={[{ required: true, message: '请输入应用名称' }]}>
              <Input />
            </Form.Item>
            <Form.Item name="appVersion" label="应用版本">
              <Input disabled />
            </Form.Item>
            <Form.Item name="apiEndpoint" label="API 地址" rules={[{ required: true, message: '请输入 API 地址' }]}>
              <Input />
            </Form.Item>
          </Card>

          <Card title="数据库设置">
            <Form.Item name="databaseHost" label="数据库主机" rules={[{ required: true, message: '请输入数据库主机' }]}>
              <Input />
            </Form.Item>
            <Form.Item name="databasePort" label="数据库端口" rules={[{ required: true, message: '请输入数据库端口' }]}>
              <Input />
            </Form.Item>
            <Form.Item name="databaseName" label="数据库名称" rules={[{ required: true, message: '请输入数据库名称' }]}>
              <Input />
            </Form.Item>
            <Form.Item name="databaseUsername" label="数据库用户名" rules={[{ required: true, message: '请输入数据库用户名' }]}>
              <Input />
            </Form.Item>
            <Form.Item name="databasePassword" label="数据库密码">
              <Input.Password />
            </Form.Item>
          </Card>

          <Card title="功能设置">
            <Form.Item name="enableLogging" label="启用日志">
              <Switch />
            </Form.Item>
            <Form.Item name="enableMonitoring" label="启用监控">
              <Switch />
            </Form.Item>
            <Form.Item name="enableGithubIntegration" label="启用 GitHub 集成">
              <Switch />
            </Form.Item>
            <Form.Item name="logLevel" label="日志级别" rules={[{ required: true, message: '请选择日志级别' }]}>
              <Select>
                <Option value="debug">Debug</Option>
                <Option value="info">Info</Option>
                <Option value="warn">Warn</Option>
                <Option value="error">Error</Option>
              </Select>
            </Form.Item>
            <Form.Item name="maxConcurrentTasks" label="最大并发任务数" rules={[{ required: true, message: '请输入最大并发任务数' }]}>
              <Input />
            </Form.Item>
          </Card>

          <Space className="action-buttons">
            <Button type="primary" icon={<SaveOutlined />} htmlType="submit" loading={isLoading}>
              保存设置
            </Button>
            <Button icon={<ReloadOutlined />} onClick={handleReset}>
              重置
            </Button>
          </Space>
        </Form>
      </Card>
    </div>
  )
}

export default Settings