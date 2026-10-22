import { BrowserRouter as Router, Routes, Route, Navigate } from 'react-router-dom'
import { ConfigProvider } from 'antd'
import zhCN from 'antd/locale/zh_CN'
import Login from './pages/Login.tsx'
import Dashboard from './pages/Dashboard.tsx'
import Automation from './pages/Automation.tsx'
import DataManagement from './pages/DataManagement.tsx'
import GitHubIntegration from './pages/GitHubIntegration.tsx'
import Settings from './pages/Settings.tsx'
import WorkOrders from './pages/WorkOrders.tsx'
import WorkOrderDetail from './pages/WorkOrderDetail.tsx'
import WorkOrderCreate from './pages/WorkOrderCreate.tsx'
import Notes from './pages/Notes.tsx'
import Requirements from './pages/Requirements.tsx'
import DailyReport from './pages/DailyReport.tsx'
import ProtectedRoute from './components/ProtectedRoute.tsx'
import Header from './components/Header.tsx'
import Sidebar from './components/Sidebar.tsx'
import './App.css'

function App() {
  return (
    <ConfigProvider locale={zhCN}>
      <Router>
        <Routes>
          <Route path="/login" element={<Login />} />
          <Route 
            path="/" 
            element={
              <ProtectedRoute>
                <div className="app-container">
                  <Sidebar />
                  <div className="main-content">
                    <Header />
                    <div className="content-area">
                      <Routes>
                        <Route path="dashboard" element={<Dashboard />} />
                        <Route path="work-orders" element={<WorkOrders />} />
                        <Route path="work-orders/create" element={<WorkOrderCreate />} />
                        <Route path="work-orders/:id" element={<WorkOrderDetail />} />
                        <Route path="automation" element={<Automation />} />
                        <Route path="data" element={<DataManagement />} />
                        <Route path="github" element={<GitHubIntegration />} />
                        <Route path="settings" element={<Settings />} />
                        <Route path="notes" element={<Notes />} />
                        <Route path="requirements" element={<Requirements />} />
                        <Route path="daily-report" element={<DailyReport />} />
                        <Route path="" element={<Navigate to="notes" replace />} />
                      </Routes>
                    </div>
                  </div>
                </div>
              </ProtectedRoute>
            } 
          />
        </Routes>
      </Router>
    </ConfigProvider>
  )
}

export default App