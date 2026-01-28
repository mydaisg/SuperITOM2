import React, { useState } from 'react'
import { Button, Checkbox, Form, Input, Card, Typography, message } from 'antd'
import { UserOutlined, LockOutlined } from '@ant-design/icons'
import { useNavigate } from 'react-router-dom'
import axios from 'axios'
import './Login.css'

const { Title } = Typography

const Login: React.FC = () => {
  const [loading, setLoading] = useState(false)
  const navigate = useNavigate()

  const handleSubmit = async (values: { username: string; password: string }) => {
    setLoading(true)
    try {
      const response = await axios.post('http://localhost:3000/auth/login', values)
      const { accessToken, user } = response.data
      
      // 存储 token 和用户信息到本地存储
      localStorage.setItem('token', accessToken)
      localStorage.setItem('user', JSON.stringify(user))
      
      message.success('登录成功')
      navigate('/dashboard')
    } catch (error: any) {
      message.error(error.response?.data?.message || '登录失败，请检查用户名和密码')
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="login-container">
      <Card className="login-card">
        <Title level={2} className="login-title">SuperITOM_TS</Title>
        <Form
          name="login"
          onFinish={handleSubmit}
          className="login-form"
        >
          <Form.Item
            name="username"
            rules={[{ required: true, message: '请输入用户名' }]}
          >
            <Input prefix={<UserOutlined />} placeholder="用户名" />
          </Form.Item>
          <Form.Item
            name="password"
            rules={[{ required: true, message: '请输入密码' }]}
          >
            <Input prefix={<LockOutlined />} type="password" placeholder="密码" />
          </Form.Item>
          <Form.Item>
            <Form.Item name="remember" valuePropName="checked" noStyle>
              <Checkbox>记住我</Checkbox>
            </Form.Item>
          </Form.Item>
          <Form.Item>
            <Button type="primary" htmlType="submit" loading={loading} className="login-button">
              登录
            </Button>
          </Form.Item>
        </Form>
      </Card>
    </div>
  )
}

export default Login