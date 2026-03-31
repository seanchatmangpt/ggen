/**
 * Main dashboard page showing customer onboarding progress and value metrics
 */

import React, { useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { api } from '../services/api';
import { useAuthStore } from '../stores/authStore';
import { customerStore } from '../stores/customerStore';
import * as Types from '../../../shared/types';
import ProgressCard from '../components/ProgressCard';
import ValueCard from '../components/ValueCard';
import AlertBanner from '../components/AlertBanner';
import SetupStepsWidget from '../components/SetupStepsWidget';
import ValueTrendChart from '../components/ValueTrendChart';
import OpenTicketsWidget from '../components/OpenTicketsWidget';
import GoLiveButton from '../components/GoLiveButton';

const DashboardPage: React.FC = () => {
  const { customerId } = useAuthStore();
  const { setCustomer } = customerStore();
  const [selectedTab, setSelectedTab] = useState<'overview' | 'metrics' | 'approvals'>('overview');

  const { data: dashboard, isLoading, error } = useQuery({
    queryKey: ['dashboard', customerId],
    queryFn: () => api.getDashboardSummary(customerId!),
    refetchInterval: 30000, // Refetch every 30 seconds
  });

  const { data: valueTrends } = useQuery({
    queryKey: ['valueTrends', customerId],
    queryFn: () => api.getValueTrend(customerId!),
    refetchInterval: 60000, // Refetch every minute
  });

  useEffect(() => {
    if (dashboard) {
      setCustomer({
        id: dashboard.customerId,
        name: 'Customer Name', // Get from API
        email: 'customer@example.com', // Get from API
        status: dashboard.status,
        createdAt: new Date(),
        updatedAt: new Date(),
        setupSteps: dashboard.setupSteps as any,
      });
    }
  }, [dashboard, setCustomer]);

  if (isLoading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="w-12 h-12 rounded-full border-4 border-blue-200 border-t-blue-600 animate-spin mx-auto mb-4"></div>
          <p className="text-gray-600">Loading dashboard...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-8">
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <h3 className="text-lg font-semibold text-red-900">Error</h3>
          <p className="text-red-700 mt-2">Failed to load dashboard</p>
        </div>
      </div>
    );
  }

  if (!dashboard) return null;

  return (
    <div className="space-y-6">
      {/* Active Alerts */}
      {dashboard.activeAlerts.length > 0 && (
        <div className="space-y-2">
          {dashboard.activeAlerts.map((alert) => (
            <AlertBanner key={alert.id} alert={alert} />
          ))}
        </div>
      )}

      {/* Header */}
      <div className="flex justify-between items-start">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">Dashboard</h1>
          <p className="text-gray-600 mt-2">
            Status: <span className="font-semibold capitalize">{dashboard.status.replace(/_/g, ' ')}</span>
          </p>
        </div>
        {dashboard.status === Types.CustomerStatus.GO_LIVE_READY && (
          <GoLiveButton customerId={dashboard.customerId} />
        )}
      </div>

      {/* Tab Navigation */}
      <div className="flex space-x-4 border-b border-gray-200">
        <button
          onClick={() => setSelectedTab('overview')}
          className={`px-4 py-2 font-medium border-b-2 transition-colors ${
            selectedTab === 'overview'
              ? 'border-blue-600 text-blue-600'
              : 'border-transparent text-gray-600 hover:text-gray-900'
          }`}
        >
          Overview
        </button>
        <button
          onClick={() => setSelectedTab('metrics')}
          className={`px-4 py-2 font-medium border-b-2 transition-colors ${
            selectedTab === 'metrics'
              ? 'border-blue-600 text-blue-600'
              : 'border-transparent text-gray-600 hover:text-gray-900'
          }`}
        >
          Metrics
        </button>
        <button
          onClick={() => setSelectedTab('approvals')}
          className={`px-4 py-2 font-medium border-b-2 transition-colors ${
            selectedTab === 'approvals'
              ? 'border-blue-600 text-blue-600'
              : 'border-transparent text-gray-600 hover:text-gray-900'
          }`}
        >
          Approvals
        </button>
      </div>

      {/* Overview Tab */}
      {selectedTab === 'overview' && (
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* Progress Cards */}
          <div className="lg:col-span-2">
            <ProgressCard
              title="Onboarding Progress"
              current={dashboard.setupProgress}
              total={100}
              status={dashboard.status}
            />
          </div>

          {/* Setup Steps */}
          <div>
            <SetupStepsWidget steps={dashboard.setupSteps} />
          </div>

          {/* Current Value */}
          <div className="lg:col-span-2">
            <ValueCard
              label="Current Value"
              value={dashboard.currentValue}
              target={dashboard.targetValue}
              changePercentage={dashboard.valueChangePercentage}
            />
          </div>

          {/* Measurement Accuracy */}
          <div className="bg-white rounded-lg shadow p-6">
            <h3 className="text-lg font-semibold text-gray-900 mb-2">Measurement Accuracy</h3>
            <div className="flex items-center">
              <div className="flex-1">
                <div className="bg-gray-200 rounded-full h-3">
                  <div
                    className="bg-green-500 h-3 rounded-full transition-all duration-300"
                    style={{ width: `${dashboard.measurementAccuracy}%` }}
                  ></div>
                </div>
              </div>
              <span className="ml-4 text-2xl font-bold text-green-600">
                {dashboard.measurementAccuracy}%
              </span>
            </div>
            <p className="text-sm text-gray-600 mt-2">
              {dashboard.measurementAccuracy >= 95
                ? 'Excellent - Ready for approval'
                : 'Good - On track for approval'}
            </p>
          </div>

          {/* Open Tickets */}
          <OpenTicketsWidget count={dashboard.openTickets} />
        </div>
      )}

      {/* Metrics Tab */}
      {selectedTab === 'metrics' && (
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          {valueTrends && (
            <div className="lg:col-span-2">
              <ValueTrendChart trends={valueTrends} />
            </div>
          )}

          {/* Detailed Metrics */}
          <div className="bg-white rounded-lg shadow p-6">
            <h3 className="text-lg font-semibold text-gray-900 mb-4">Key Metrics</h3>
            <div className="space-y-4">
              <div className="flex justify-between">
                <span className="text-gray-600">Current Value</span>
                <span className="font-semibold">{dashboard.currentValue.toFixed(2)}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-600">Target Value</span>
                <span className="font-semibold">{dashboard.targetValue.toFixed(2)}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-600">Change</span>
                <span
                  className={`font-semibold ${
                    dashboard.valueChangePercentage >= 0 ? 'text-green-600' : 'text-red-600'
                  }`}
                >
                  {dashboard.valueChangePercentage >= 0 ? '+' : ''}
                  {dashboard.valueChangePercentage.toFixed(2)}%
                </span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-600">Last Update</span>
                <span className="font-semibold">
                  {new Date(dashboard.lastMeasurementAt).toLocaleDateString()}
                </span>
              </div>
            </div>
          </div>

          {/* Next Measurement */}
          <div className="bg-white rounded-lg shadow p-6">
            <h3 className="text-lg font-semibold text-gray-900 mb-4">Next Update</h3>
            <p className="text-gray-600 mb-4">
              {new Date(dashboard.nextMeasurementAt).toLocaleDateString()}{' '}
              at {new Date(dashboard.nextMeasurementAt).toLocaleTimeString()}
            </p>
            <div className="flex items-center space-x-3">
              <div className="flex-1">
                <div className="bg-gray-200 rounded-full h-2">
                  <div
                    className="bg-blue-500 h-2 rounded-full"
                    style={{
                      width: `${
                        (100 *
                          (Date.now() - new Date(dashboard.lastMeasurementAt).getTime())) /
                        (new Date(dashboard.nextMeasurementAt).getTime() -
                          new Date(dashboard.lastMeasurementAt).getTime())
                      }%`,
                    }}
                  ></div>
                </div>
              </div>
              <span className="text-sm text-gray-600">In progress</span>
            </div>
          </div>
        </div>
      )}

      {/* Approvals Tab */}
      {selectedTab === 'approvals' && (
        <div className="bg-white rounded-lg shadow p-6">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Approval Status</h3>
          <div className="space-y-4">
            {Object.entries(dashboard.approvalsStatus).map(([stage, approved]) => (
              <div key={stage} className="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                <span className="text-gray-700 capitalize">{stage.replace(/_/g, ' ')}</span>
                <div
                  className={`px-3 py-1 rounded-full text-sm font-medium ${
                    approved
                      ? 'bg-green-100 text-green-800'
                      : 'bg-yellow-100 text-yellow-800'
                  }`}
                >
                  {approved ? 'Approved' : 'Pending'}
                </div>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
};

export default DashboardPage;
