import React from 'react'
import { Layout, Typography, Space, Button, Avatar, Dropdown, Menu } from 'antd'
import { UserOutlined, LogoutOutlined, SettingOutlined } from '@ant-design/icons'
import { useNavigate } from 'react-router-dom'
import './Header.css'

const { Header: AntHeader } = Layout
const { Title } = Typography

const Header: React.FC = () => {
  const navigate = useNavigate()
  const user = JSON.parse(localStorage.getItem('user') || '{}')

  const handleLogout = () => {
    // 清除本地存储的 token 和用户信息
    localStorage.removeItem('token')
    localStorage.removeItem('user')
    // 重定向到登录页面
    navigate('/login')
  }

  const handleSettings = () => {
    navigate('/settings')
  }

  const menu = (
    <Menu>
      <Menu.Item key="settings" icon={<SettingOutlined />} onClick={handleSettings}>
        设置
      </Menu.Item>
      <Menu.Item key="logout" icon={<LogoutOutlined />} onClick={handleLogout}>
        退出登录
      </Menu.Item>
    </Menu>
  )

  return (
    <AntHeader className="app-header">
      <div className="header-left">
        <Title level={4} className="app-title">SuperITOM_TS</Title>
      </div>
      <div className="header-right">
        <Space>
          <span className="user-info">
            欢迎，{user.username || '用户'}
          </span>
          <Dropdown overlay={menu} placement="bottomRight">
            <Avatar icon={<UserOutlined />} className="user-avatar" />
          </Dropdown>
        </Space>
      </div>
    </AntHeader>
  )
}

export default Header