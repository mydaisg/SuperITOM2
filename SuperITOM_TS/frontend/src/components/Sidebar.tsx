import React from 'react'
import { Layout, Menu, Typography } from 'antd'
import { FileTextOutlined, ProfileOutlined, CalendarOutlined } from '@ant-design/icons'
import { Link, useLocation } from 'react-router-dom'
import './Sidebar.css'

const { Sider } = Layout
const { Title } = Typography

const Sidebar: React.FC = () => {
  const location = useLocation()
  const pathSegments = location.pathname.split('/').filter(Boolean)
  const currentPath = pathSegments[0] || 'notes'

  const menuItems = [
    {
      key: 'notes',
      icon: <FileTextOutlined />,
      label: <Link to="/notes">记事</Link>,
    },
    {
      key: 'requirements',
      icon: <ProfileOutlined />,
      label: <Link to="/requirements">需求</Link>,
    },
    {
      key: 'daily-report',
      icon: <CalendarOutlined />,
      label: <Link to="/daily-report">总结</Link>,
    },
  ]

  return (
    <Sider width={200} className="app-sidebar">
      <div className="sidebar-header">
        <Title level={5} className="sidebar-title">SuperITOM_TS</Title>
      </div>
      <Menu
        mode="inline"
        selectedKeys={[currentPath]}
        items={menuItems}
        className="sidebar-menu"
      />
    </Sider>
  )
}

export default Sidebar