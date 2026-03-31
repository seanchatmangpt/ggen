/**
 * Main App component with routing
 */

import React, { useEffect } from 'react';
import { BrowserRouter as Router, Routes, Route, Navigate } from 'react-router-dom';
import { QueryClientProvider } from '@tanstack/react-query';
import { queryClient } from './services/queryClient';
import Layout from './components/Layout';
import LoginPage from './pages/LoginPage';
import SetupWizardPage from './pages/SetupWizardPage';
import ValueDefinitionPage from './pages/ValueDefinitionPage';
import ApprovalPage from './pages/ApprovalPage';
import DashboardPage from './pages/DashboardPage';
import SupportPage from './pages/SupportPage';
import { useAuthStore } from './stores/authStore';

const App: React.FC = () => {
  const { token, isInitialized, initialize } = useAuthStore();

  useEffect(() => {
    initialize();
  }, [initialize]);

  if (!isInitialized) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-100">
        <div className="text-center">
          <div className="w-12 h-12 rounded-full border-4 border-blue-200 border-t-blue-600 animate-spin mx-auto mb-4"></div>
          <p className="text-gray-600">Loading...</p>
        </div>
      </div>
    );
  }

  return (
    <QueryClientProvider client={queryClient}>
      <Router>
        <Routes>
          <Route path="/login" element={<LoginPage />} />
          <Route
            path="/*"
            element={
              token ? (
                <Layout>
                  <Routes>
                    <Route path="/" element={<Navigate to="/dashboard" replace />} />
                    <Route path="/dashboard" element={<DashboardPage />} />
                    <Route path="/setup" element={<SetupWizardPage />} />
                    <Route path="/value-definitions" element={<ValueDefinitionPage />} />
                    <Route path="/approvals" element={<ApprovalPage />} />
                    <Route path="/support" element={<SupportPage />} />
                    <Route path="*" element={<Navigate to="/dashboard" replace />} />
                  </Routes>
                </Layout>
              ) : (
                <Navigate to="/login" replace />
              )
            }
          />
        </Routes>
      </Router>
    </QueryClientProvider>
  );
};

export default App;
