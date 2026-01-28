import React, { useState, useEffect } from 'react'
import { Card, Typography, Table, Button, Select, Input, message, Space, Tag } from 'antd'
import { PlayCircleOutlined, StopOutlined, ReloadOutlined, FileTextOutlined } from '@ant-design/icons'
import axios from 'axios'
import './Automation.css'

const { Title } = Typography
const { Option } = Select
const { TextArea } = Input

const Automation: React.FC = () => {
  const [scripts, setScripts] = useState<any[]>([])
  const [selectedScript, setSelectedScript] = useState<string>('')
  const [scriptArgs, setScriptArgs] = useState<string>('')
  const [executionOutput, setExecutionOutput] = useState<string>('')
  const [isExecuting, setIsExecuting] = useState(false)
  const [executionId, setExecutionId] = useState<string>('')

  useEffect(() => {
    fetchScripts()
  }, [])

  const fetchScripts = async () => {
    try {
      const response = await axios.get('http://localhost:3000/automation/scripts')
      setScripts(response.data)
    } catch (error) {
      message.error('获取脚本列表失败')
    }
  }

  const handleExecute = async () => {
    if (!selectedScript) {
      message.warning('请选择脚本')
      return
    }

    setIsExecuting(true)
    setExecutionOutput('')

    try {
      const args = scriptArgs.split(' ').filter(arg => arg.trim() !== '')
      const response = await axios.post('http://localhost:3000/automation/execute', {
        script: selectedScript,
        args,
      })

      // 处理流式响应
      const reader = response.data.getReader()
      const decoder = new TextDecoder()

      while (true) {
        const { done, value } = await reader.read()
        if (done) break
        const chunk = decoder.decode(value)
        setExecutionOutput(prev => prev + chunk)
      }

      message.success('脚本执行完成')
    } catch (error) {
      message.error('脚本执行失败')
    } finally {
      setIsExecuting(false)
    }
  }

  const handleStop = async () => {
    if (!executionId) {
      message.warning('没有正在执行的脚本')
      return
    }

    try {
      await axios.post(`http://localhost:3000/automation/terminate/${executionId}`)
      message.success('脚本已终止')
      setIsExecuting(false)
    } catch (error) {
      message.error('终止脚本失败')
    }
  }

  return (
    <div className="automation-container">
      <Title level={2}>自动化管理</Title>

      <Card className="automation-card">
        <div className="script-selector">
          <div className="form-group">
            <label>选择脚本</label>
            <Select
              style={{ width: '100%' }}
              placeholder="请选择要执行的脚本"
              value={selectedScript}
              onChange={setSelectedScript}
            >
              {scripts.map(script => (
                <Option key={script.name} value={script.name}>
                  {script.name}
                </Option>
              ))}
            </Select>
          </div>

          <div className="form-group">
            <label>脚本参数</label>
            <Input
              placeholder="输入脚本参数，多个参数用空格分隔"
              value={scriptArgs}
              onChange={(e) => setScriptArgs(e.target.value)}
            />
          </div>

          <Space className="action-buttons">
            <Button
              type="primary"
              icon={<PlayCircleOutlined />}
              onClick={handleExecute}
              disabled={isExecuting || !selectedScript}
            >
              执行脚本
            </Button>
            <Button
              icon={<StopOutlined />}
              onClick={handleStop}
              disabled={!isExecuting}
            >
              终止执行
            </Button>
            <Button
              icon={<ReloadOutlined />}
              onClick={fetchScripts}
            >
              刷新脚本
            </Button>
          </Space>
        </div>

        <div className="execution-output">
          <Typography.Title level={4}>执行输出</Typography.Title>
          <TextArea
            value={executionOutput}
            rows={10}
            readOnly
            className="output-textarea"
          />
        </div>
      </Card>

      <Card title="脚本列表" className="scripts-list-card">
        <Table dataSource={scripts} rowKey="name" pagination={false}>
          <Table.Column
            title="脚本名称"
            dataIndex="name"
            key="name"
            render={(name) => (
              <Space>
                <FileTextOutlined />
                <span>{name}</span>
              </Space>
            )}
          />
          <Table.Column
            title="路径"
            dataIndex="path"
            key="path"
            ellipsis
          />
          <Table.Column
            title="操作"
            key="action"
            render={(record) => (
              <Button
                type="link"
                onClick={() => setSelectedScript(record.name)}
              >
                选择
              </Button>
            )}
          />
        </Table>
      </Card>
    </div>
  )
}

export default Automation