import React, { useState, useEffect } from 'react'
import { Card, Row, Col, Statistic, Typography, Table, Tag, List, Avatar } from 'antd'
import { UserOutlined, ScriptOutlined, DatabaseOutlined, GitBranchOutlined, BarChartOutlined } from '@ant-design/icons'
import axios from 'axios'
import './Dashboard.css'

const { Title, Text } = Typography
const { Column } = Table

const Dashboard: React.FC = () => {
  const [systemStats, setSystemStats] = useState({
    users: 0,
    scripts: 0,
    dataPoints: 0,
    githubRepos: 0,
  })
  const [recentActivities, setRecentActivities] = useState<any[]>([])
  const [automationTasks, setAutomationTasks] = useState<any[]>([])

  useEffect(() => {
    // 模拟获取系统统计数据
    setSystemStats({
      users: 120,
      scripts: 45,
      dataPoints: 15000,
      githubRepos: 18,
    })

    // 模拟最近活动数据
    setRecentActivities([
      { id: 1, user: 'admin', action: '执行脚本', target: 'backup.ps1', time: '2026-01-28 10:30' },
      { id: 2, user: 'user1', action: '登录系统', target: 'Web界面', time: '2026-01-28 09:15' },
      { id: 3, user: 'admin', action: '修改配置', target: '系统设置', time: '2026-01-27 16:45' },
      { id: 4, user: 'user2', action: '执行脚本', target: 'monitor.ps1', time: '2026-01-27 14:20' },
    ])

    // 模拟自动化任务数据
    setAutomationTasks([
      { id: 1, name: '备份系统', status: 'success', progress: 100, startTime: '2026-01-28 10:00', endTime: '2026-01-28 10:15' },
      { id: 2, name: '监控服务', status: 'running', progress: 65, startTime: '2026-01-28 09:30', endTime: null },
      { id: 3, name: '清理日志', status: 'success', progress: 100, startTime: '2026-01-28 08:45', endTime: '2026-01-28 09:00' },
      { id: 4, name: '更新软件', status: 'failed', progress: 30, startTime: '2026-01-28 08:00', endTime: '2026-01-28 08:10' },
    ])
  }, [])

  const getStatusTag = (status: string) => {
    switch (status) {
      case 'success':
        return <Tag color="green">成功</Tag>
      case 'running':
        return <Tag color="blue">运行中</Tag>
      case 'failed':
        return <Tag color="red">失败</Tag>
      default:
        return <Tag color="gray">未知</Tag>
    }
  }

  return (
    <div className="dashboard-container">
      <Title level={2}>仪表板</Title>
      
      {/* 系统统计 */}
      <Row gutter={[16, 16]} className="stats-row">
        <Col xs={24} sm={12} md={6}>
          <Card>
            <Statistic 
              title="用户数量" 
              value={systemStats.users} 
              prefix={<UserOutlined />} 
              suffix="人"
            />
          </Card>
        </Col>
        <Col xs={24} sm={12} md={6}>
          <Card>
            <Statistic 
              title="脚本数量" 
              value={systemStats.scripts} 
              prefix={<ScriptOutlined />} 
              suffix="个"
            />
          </Card>
        </Col>
        <Col xs={24} sm={12} md={6}>
          <Card>
            <Statistic 
              title="数据点" 
              value={systemStats.dataPoints} 
              prefix={<DatabaseOutlined />} 
              suffix="个"
            />
          </Card>
        </Col>
        <Col xs={24} sm={12} md={6}>
          <Card>
            <Statistic 
              title="GitHub 仓库" 
              value={systemStats.githubRepos} 
              prefix={<GitBranchOutlined />} 
              suffix="个"
            />
          </Card>
        </Col>
      </Row>

      <Row gutter={[16, 16]} className="content-row">
        {/* 自动化任务 */}
        <Col xs={24} md={12}>
          <Card title="自动化任务" extra={<BarChartOutlined />}>
            <Table dataSource={automationTasks} rowKey="id" pagination={false}>
              <Column title="任务名称" dataIndex="name" key="name" />
              <Column 
                title="状态" 
                dataIndex="status" 
                key="status" 
                render={(status) => getStatusTag(status)}
              />
              <Column 
                title="进度" 
                dataIndex="progress" 
                key="progress" 
                render={(progress) => (
                  <div className="progress-bar">
                    <div className="progress-fill" style={{ width: `${progress}%` }}></div>
                    <span>{progress}%</span>
                  </div>
                )}
              />
              <Column 
                title="开始时间" 
                dataIndex="startTime" 
                key="startTime" 
              />
            </Table>
          </Card>
        </Col>

        {/* 最近活动 */}
        <Col xs={24} md={12}>
          <Card title="最近活动">
            <List
              itemLayout="horizontal"
              dataSource={recentActivities}
              renderItem={(item) => (
                <List.Item>
                  <List.Item.Meta
                    avatar={<Avatar icon={<UserOutlined />} />}
                    title={
                      <Text>
                        {item.user} {item.action} {item.target}
                      </Text>
                    }
                    description={item.time}
                  />
                </List.Item>
              )}
            />
          </Card>
        </Col>
      </Row>
    </div>
  )
}

export default Dashboard