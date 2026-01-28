import React, { useState, useEffect } from 'react'
import { Card, Typography, Table, Button, Input, message, Space, Tag, List, Avatar } from 'antd'
import { GitBranchOutlined, SyncOutlined, PlusOutlined, DeleteOutlined, EyeOutlined } from '@ant-design/icons'
import axios from 'axios'
import './GitHubIntegration.css'

const { Title } = Typography

const GitHubIntegration: React.FC = () => {
  const [repos, setRepos] = useState<any[]>([])
  const [webhooks, setWebhooks] = useState<any[]>([])
  const [repoUrl, setRepoUrl] = useState<string>('')
  const [isSyncing, setIsSyncing] = useState(false)

  // 模拟 GitHub 仓库数据
  useState(() => {
    setRepos([
      { id: 1, name: 'SuperITOM2', url: 'https://github.com/superitom/SuperITOM2', branch: 'main', status: 'active' },
      { id: 2, name: 'SuperITOM_TS', url: 'https://github.com/superitom/SuperITOM_TS', branch: 'main', status: 'active' },
      { id: 3, name: 'automation-scripts', url: 'https://github.com/superitom/automation-scripts', branch: 'main', status: 'inactive' },
      { id: 4, name: 'monitoring-tools', url: 'https://github.com/superitom/monitoring-tools', branch: 'main', status: 'active' },
    ])

    setWebhooks([
      { id: 1, repo: 'SuperITOM2', event: 'push', url: 'http://localhost:3000/github/webhook', status: 'active' },
      { id: 2, repo: 'SuperITOM_TS', event: 'push', url: 'http://localhost:3000/github/webhook', status: 'active' },
      { id: 3, repo: 'SuperITOM2', event: 'pull_request', url: 'http://localhost:3000/github/webhook', status: 'inactive' },
    ])
  })

  const handleSync = async () => {
    setIsSyncing(true)
    // 模拟同步 GitHub 仓库
    setTimeout(() => {
      message.success('GitHub 仓库同步成功')
      setIsSyncing(false)
    }, 1000)
  }

  const handleAddRepo = () => {
    if (!repoUrl) {
      message.warning('请输入仓库 URL')
      return
    }

    // 模拟添加仓库
    const newRepo = {
      id: repos.length + 1,
      name: repoUrl.split('/').pop() || '',
      url: repoUrl,
      branch: 'main',
      status: 'active',
    }

    setRepos([...repos, newRepo])
    setRepoUrl('')
    message.success('仓库添加成功')
  }

  const handleDeleteRepo = (repo: any) => {
    // 模拟删除仓库
    setRepos(repos.filter(r => r.id !== repo.id))
    message.success(`删除仓库 ${repo.name} 成功`)
  }

  const handleViewRepo = (repo: any) => {
    // 模拟查看仓库
    window.open(repo.url, '_blank')
  }

  const getStatusTag = (status: string) => {
    return status === 'active' ? 
      <Tag color="green">活跃</Tag> : 
      <Tag color="gray">非活跃</Tag>
  }

  return (
    <div className="github-integration-container">
      <Title level={2}>GitHub 集成</Title>

      <Card className="github-card">
        <div className="repo-add-section">
          <div className="form-group">
            <label>添加 GitHub 仓库</label>
            <Space>
              <Input
                placeholder="输入 GitHub 仓库 URL"
                value={repoUrl}
                onChange={(e) => setRepoUrl(e.target.value)}
                style={{ flex: 1 }}
              />
              <Button type="primary" icon={<PlusOutlined />} onClick={handleAddRepo}>
                添加
              </Button>
            </Space>
          </div>

          <Button
            icon={<SyncOutlined />}
            loading={isSyncing}
            onClick={handleSync}
            className="sync-button"
          >
            同步仓库
          </Button>
        </div>

        <Card title="仓库列表" className="repos-list-card">
          <Table dataSource={repos} rowKey="id" pagination={false}>
            <Table.Column
              title="仓库名称"
              dataIndex="name"
              key="name"
              render={(name, record) => (
                <Space>
                  <GitBranchOutlined />
                  <span>{name}</span>
                </Space>
              )}
            />
            <Table.Column
              title="URL"
              dataIndex="url"
              key="url"
              ellipsis
              render={(url) => <a href={url} target="_blank" rel="noopener noreferrer">{url}</a>}
            />
            <Table.Column
              title="分支"
              dataIndex="branch"
              key="branch"
            />
            <Table.Column
              title="状态"
              dataIndex="status"
              key="status"
              render={(status) => getStatusTag(status)}
            />
            <Table.Column
              title="操作"
              key="action"
              render={(text, record) => (
                <Space>
                  <Button icon={<EyeOutlined />} onClick={() => handleViewRepo(record)}>
                    查看
                  </Button>
                  <Button danger icon={<DeleteOutlined />} onClick={() => handleDeleteRepo(record)}>
                    删除
                  </Button>
                </Space>
              )}
            />
          </Table>
        </Card>

        <Card title="Webhook 配置" className="webhooks-list-card">
          <List
            itemLayout="horizontal"
            dataSource={webhooks}
            renderItem={(webhook) => (
              <List.Item>
                <List.Item.Meta
                  avatar={<Avatar icon={<GitBranchOutlined />} />}
                  title={
                    <Space>
                      <span>{webhook.repo}</span>
                      <Tag>{webhook.event}</Tag>
                      {getStatusTag(webhook.status)}
                    </Space>
                  }
                  description={webhook.url}
                />
              </List.Item>
            )}
          />
        </Card>
      </Card>
    </div>
  )
}

export default GitHubIntegration