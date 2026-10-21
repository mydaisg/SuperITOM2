import React from 'react'
import { Layout, Menu, Typography } from 'antd'
import { DashboardOutlined, CodeOutlined, DatabaseOutlined, BranchesOutlined, SettingOutlined, FileTextOutlined } from '@ant-design/icons'
import { Link, useLocation } from 'react-router-dom'
import './Sidebar.css'

const { Sider } = Layout
const { Title } = Typography

const Sidebar: React.FC = () => {
  const location = useLocation()
  const pathSegments = location.pathname.split('/').filter(Boolean)
  const currentPath = pathSegments[0] || 'dashboard'

  const menuItems = [
    {
      key: 'dashboard',
      icon: <DashboardOutlined />,
      label: <Link to="/dashboard">仪表板</Link>,
    },
    {
      key: 'work-orders',
      icon: <FileTextOutlined />,
      label: <Link to="/work-orders">工单</Link>,
    },
    {
      key: 'automation',
      icon: <CodeOutlined />,
      label: <Link to="/automation">自动化</Link>,
    },
    {
      key: 'data',
      icon: <DatabaseOutlined />,
      label: <Link to="/data">数据管理</Link>,
    },
    {
      key: 'github',
      icon: <BranchesOutlined />,
      label: <Link to="/github">GitHub 集成</Link>,
    },
    {
      key: 'settings',
      icon: <SettingOutlined />,
      label: <Link to="/settings">设置</Link>,
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