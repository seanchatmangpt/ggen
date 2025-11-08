// ============================================================================
// Enterprise ERP Core - Dashboard UI (TypeScript/React)
// Financial reporting and analytics dashboard
// ============================================================================

import React, { useState, useEffect } from 'react';
import {
  BarChart, Bar, LineChart, Line, PieChart, Pie,
  XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer
} from 'recharts';

// ----------------------------------------------------------------------------
// Types
// ----------------------------------------------------------------------------

interface Account {
  accountNumber: string;
  accountName: string;
  accountType: 'Asset' | 'Liability' | 'Equity' | 'Revenue' | 'Expense';
  balance: number;
}

interface TrialBalance {
  asOfDate: string;
  accounts: Array<{
    accountNumber: string;
    accountName: string;
    debitTotal: number;
    creditTotal: number;
    balance: number;
  }>;
}

interface AgingReport {
  items: Array<{
    name: string;
    invoiceNumber: string;
    dueDate: string;
    amountDue: number;
    daysPastDue: number;
    agingBucket: string;
  }>;
}

interface FinancialStatement {
  type: 'income' | 'balance' | 'cashflow';
  startDate: string;
  endDate: string;
  sections: Array<{
    name: string;
    accounts: Array<{
      accountNumber: string;
      accountName: string;
      amount: number;
    }>;
    subtotal: number;
  }>;
  grandTotal: number;
}

// ----------------------------------------------------------------------------
// Dashboard Component
// ----------------------------------------------------------------------------

export const ERPDashboard: React.FC = () => {
  const [selectedReport, setSelectedReport] = useState<string>('overview');
  const [dateRange, setDateRange] = useState({
    startDate: '2025-01-01',
    endDate: '2025-01-31'
  });

  return (
    <div className="erp-dashboard">
      <header className="dashboard-header">
        <h1>Enterprise ERP Dashboard</h1>
        <nav>
          <button onClick={() => setSelectedReport('overview')}>Overview</button>
          <button onClick={() => setSelectedReport('income')}>Income Statement</button>
          <button onClick={() => setSelectedReport('balance')}>Balance Sheet</button>
          <button onClick={() => setSelectedReport('aging')}>A/R Aging</button>
          <button onClick={() => setSelectedReport('cashflow')}>Cash Flow</button>
        </nav>
      </header>

      <div className="dashboard-content">
        {selectedReport === 'overview' && <OverviewPanel dateRange={dateRange} />}
        {selectedReport === 'income' && <IncomeStatementPanel dateRange={dateRange} />}
        {selectedReport === 'balance' && <BalanceSheetPanel dateRange={dateRange} />}
        {selectedReport === 'aging' && <AgingReportPanel />}
        {selectedReport === 'cashflow' && <CashFlowPanel dateRange={dateRange} />}
      </div>
    </div>
  );
};

// ----------------------------------------------------------------------------
// Overview Panel
// ----------------------------------------------------------------------------

const OverviewPanel: React.FC<{ dateRange: any }> = ({ dateRange }) => {
  const [metrics, setMetrics] = useState({
    totalRevenue: 0,
    totalExpenses: 0,
    netIncome: 0,
    totalAssets: 0,
    totalLiabilities: 0,
    cashBalance: 0
  });

  useEffect(() => {
    // Fetch metrics from backend (simulated)
    setMetrics({
      totalRevenue: 125000,
      totalExpenses: 87500,
      netIncome: 37500,
      totalAssets: 450000,
      totalLiabilities: 125000,
      cashBalance: 75000
    });
  }, [dateRange]);

  const revenueData = [
    { month: 'Jan', revenue: 25000, expenses: 17500 },
    { month: 'Feb', revenue: 28000, expenses: 18000 },
    { month: 'Mar', revenue: 32000, expenses: 19000 },
    { month: 'Apr', revenue: 40000, expenses: 33000 }
  ];

  return (
    <div className="overview-panel">
      <div className="metrics-grid">
        <MetricCard title="Total Revenue" value={metrics.totalRevenue} format="currency" trend="+12%" />
        <MetricCard title="Total Expenses" value={metrics.totalExpenses} format="currency" trend="+8%" />
        <MetricCard title="Net Income" value={metrics.netIncome} format="currency" trend="+18%" />
        <MetricCard title="Cash Balance" value={metrics.cashBalance} format="currency" />
      </div>

      <div className="charts-grid">
        <div className="chart-container">
          <h3>Revenue vs Expenses</h3>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={revenueData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="month" />
              <YAxis />
              <Tooltip formatter={(value) => `$${value.toLocaleString()}`} />
              <Legend />
              <Bar dataKey="revenue" fill="#4CAF50" name="Revenue" />
              <Bar dataKey="expenses" fill="#F44336" name="Expenses" />
            </BarChart>
          </ResponsiveContainer>
        </div>

        <div className="chart-container">
          <h3>Financial Position</h3>
          <ResponsiveContainer width="100%" height={300}>
            <PieChart>
              <Pie
                data={[
                  { name: 'Assets', value: metrics.totalAssets },
                  { name: 'Liabilities', value: metrics.totalLiabilities },
                  { name: 'Equity', value: metrics.totalAssets - metrics.totalLiabilities }
                ]}
                dataKey="value"
                nameKey="name"
                cx="50%"
                cy="50%"
                outerRadius={80}
                fill="#8884d8"
                label={(entry) => `${entry.name}: $${entry.value.toLocaleString()}`}
              />
              <Tooltip formatter={(value) => `$${value.toLocaleString()}`} />
            </PieChart>
          </ResponsiveContainer>
        </div>
      </div>
    </div>
  );
};

// ----------------------------------------------------------------------------
// Income Statement Panel
// ----------------------------------------------------------------------------

const IncomeStatementPanel: React.FC<{ dateRange: any }> = ({ dateRange }) => {
  const [statement, setStatement] = useState<FinancialStatement | null>(null);

  useEffect(() => {
    // Fetch income statement from backend (simulated)
    setStatement({
      type: 'income',
      startDate: dateRange.startDate,
      endDate: dateRange.endDate,
      sections: [
        {
          name: 'Revenue',
          accounts: [
            { accountNumber: '4000', accountName: 'Sales Revenue', amount: 125000 },
            { accountNumber: '4100', accountName: 'Service Revenue', amount: 35000 }
          ],
          subtotal: 160000
        },
        {
          name: 'Expenses',
          accounts: [
            { accountNumber: '5000', accountName: 'Cost of Goods Sold', amount: 65000 },
            { accountNumber: '5100', accountName: 'Salaries & Wages', amount: 40000 },
            { accountNumber: '5200', accountName: 'Rent', amount: 12000 },
            { accountNumber: '5300', accountName: 'Utilities', amount: 3500 }
          ],
          subtotal: 120500
        }
      ],
      grandTotal: 39500
    });
  }, [dateRange]);

  if (!statement) return <div>Loading...</div>;

  return (
    <div className="income-statement-panel">
      <h2>Income Statement</h2>
      <p className="date-range">
        For the period {statement.startDate} to {statement.endDate}
      </p>

      {statement.sections.map((section) => (
        <div key={section.name} className="statement-section">
          <h3>{section.name}</h3>
          <table className="financial-table">
            <thead>
              <tr>
                <th>Account</th>
                <th>Account Name</th>
                <th>Amount</th>
              </tr>
            </thead>
            <tbody>
              {section.accounts.map((account) => (
                <tr key={account.accountNumber}>
                  <td>{account.accountNumber}</td>
                  <td>{account.accountName}</td>
                  <td className="amount">${account.amount.toLocaleString()}</td>
                </tr>
              ))}
              <tr className="subtotal">
                <td colSpan={2}>Total {section.name}</td>
                <td className="amount">${section.subtotal.toLocaleString()}</td>
              </tr>
            </tbody>
          </table>
        </div>
      ))}

      <div className="grand-total">
        <strong>Net Income:</strong> ${statement.grandTotal.toLocaleString()}
      </div>
    </div>
  );
};

// ----------------------------------------------------------------------------
// Balance Sheet Panel
// ----------------------------------------------------------------------------

const BalanceSheetPanel: React.FC<{ dateRange: any }> = ({ dateRange }) => {
  const [statement, setStatement] = useState<FinancialStatement | null>(null);

  useEffect(() => {
    setStatement({
      type: 'balance',
      startDate: dateRange.startDate,
      endDate: dateRange.endDate,
      sections: [
        {
          name: 'Assets',
          accounts: [
            { accountNumber: '1000', accountName: 'Cash', amount: 75000 },
            { accountNumber: '1200', accountName: 'Accounts Receivable', amount: 45000 },
            { accountNumber: '1500', accountName: 'Inventory', amount: 65000 },
            { accountNumber: '1800', accountName: 'Fixed Assets', amount: 265000 }
          ],
          subtotal: 450000
        },
        {
          name: 'Liabilities',
          accounts: [
            { accountNumber: '2000', accountName: 'Accounts Payable', amount: 32000 },
            { accountNumber: '2100', accountName: 'Notes Payable', amount: 50000 },
            { accountNumber: '2500', accountName: 'Long-term Debt', amount: 43000 }
          ],
          subtotal: 125000
        },
        {
          name: 'Equity',
          accounts: [
            { accountNumber: '3000', accountName: 'Common Stock', amount: 200000 },
            { accountNumber: '3200', accountName: 'Retained Earnings', amount: 125000 }
          ],
          subtotal: 325000
        }
      ],
      grandTotal: 450000
    });
  }, [dateRange]);

  if (!statement) return <div>Loading...</div>;

  return (
    <div className="balance-sheet-panel">
      <h2>Balance Sheet</h2>
      <p className="date-range">As of {statement.endDate}</p>

      {statement.sections.map((section) => (
        <div key={section.name} className="statement-section">
          <h3>{section.name}</h3>
          <table className="financial-table">
            <tbody>
              {section.accounts.map((account) => (
                <tr key={account.accountNumber}>
                  <td>{account.accountNumber}</td>
                  <td>{account.accountName}</td>
                  <td className="amount">${account.amount.toLocaleString()}</td>
                </tr>
              ))}
              <tr className="subtotal">
                <td colSpan={2}>Total {section.name}</td>
                <td className="amount">${section.subtotal.toLocaleString()}</td>
              </tr>
            </tbody>
          </table>
        </div>
      ))}
    </div>
  );
};

// ----------------------------------------------------------------------------
// Aging Report Panel
// ----------------------------------------------------------------------------

const AgingReportPanel: React.FC = () => {
  const [agingData, setAgingData] = useState<AgingReport>({ items: [] });

  useEffect(() => {
    setAgingData({
      items: [
        { name: 'Acme Corp', invoiceNumber: 'INV-001', dueDate: '2025-01-15', amountDue: 5000, daysPastDue: 23, agingBucket: '1-30 Days' },
        { name: 'Globex Inc', invoiceNumber: 'INV-002', dueDate: '2025-01-10', amountDue: 12000, daysPastDue: 28, agingBucket: '1-30 Days' },
        { name: 'Initech LLC', invoiceNumber: 'INV-003', dueDate: '2024-12-20', amountDue: 8500, daysPastDue: 49, agingBucket: '31-60 Days' }
      ]
    });
  }, []);

  const bucketTotals = agingData.items.reduce((acc, item) => {
    acc[item.agingBucket] = (acc[item.agingBucket] || 0) + item.amountDue;
    return acc;
  }, {} as Record<string, number>);

  return (
    <div className="aging-report-panel">
      <h2>Accounts Receivable Aging</h2>

      <div className="aging-summary">
        {Object.entries(bucketTotals).map(([bucket, total]) => (
          <div key={bucket} className="aging-bucket">
            <span className="bucket-name">{bucket}</span>
            <span className="bucket-total">${total.toLocaleString()}</span>
          </div>
        ))}
      </div>

      <table className="aging-table">
        <thead>
          <tr>
            <th>Customer</th>
            <th>Invoice</th>
            <th>Due Date</th>
            <th>Amount Due</th>
            <th>Days Past Due</th>
            <th>Aging Bucket</th>
          </tr>
        </thead>
        <tbody>
          {agingData.items.map((item, idx) => (
            <tr key={idx}>
              <td>{item.name}</td>
              <td>{item.invoiceNumber}</td>
              <td>{item.dueDate}</td>
              <td className="amount">${item.amountDue.toLocaleString()}</td>
              <td className={item.daysPastDue > 30 ? 'warning' : ''}>{item.daysPastDue}</td>
              <td>{item.agingBucket}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

// ----------------------------------------------------------------------------
// Cash Flow Panel
// ----------------------------------------------------------------------------

const CashFlowPanel: React.FC<{ dateRange: any }> = ({ dateRange }) => {
  const cashFlowData = [
    { month: 'Jan', operating: 15000, investing: -5000, financing: 0 },
    { month: 'Feb', operating: 18000, investing: -2000, financing: 10000 },
    { month: 'Mar', operating: 22000, investing: -8000, financing: 0 },
    { month: 'Apr', operating: 25000, investing: -3000, financing: -5000 }
  ];

  return (
    <div className="cashflow-panel">
      <h2>Cash Flow Statement</h2>
      <ResponsiveContainer width="100%" height={400}>
        <LineChart data={cashFlowData}>
          <CartesianGrid strokeDasharray="3 3" />
          <XAxis dataKey="month" />
          <YAxis />
          <Tooltip formatter={(value) => `$${value.toLocaleString()}`} />
          <Legend />
          <Line type="monotone" dataKey="operating" stroke="#4CAF50" name="Operating Activities" />
          <Line type="monotone" dataKey="investing" stroke="#2196F3" name="Investing Activities" />
          <Line type="monotone" dataKey="financing" stroke="#FF9800" name="Financing Activities" />
        </LineChart>
      </ResponsiveContainer>
    </div>
  );
};

// ----------------------------------------------------------------------------
// Metric Card Component
// ----------------------------------------------------------------------------

const MetricCard: React.FC<{
  title: string;
  value: number;
  format?: 'currency' | 'number';
  trend?: string;
}> = ({ title, value, format = 'number', trend }) => {
  const formattedValue = format === 'currency'
    ? `$${value.toLocaleString()}`
    : value.toLocaleString();

  return (
    <div className="metric-card">
      <h4>{title}</h4>
      <div className="metric-value">{formattedValue}</div>
      {trend && <div className="metric-trend">{trend}</div>}
    </div>
  );
};
