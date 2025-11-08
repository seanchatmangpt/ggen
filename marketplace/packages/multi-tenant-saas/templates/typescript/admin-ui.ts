// Multi-Tenant SaaS Admin UI - TypeScript/React Implementation
// Tenant management dashboard for provisioning, monitoring, and billing

import React, { useState, useEffect } from 'react';

interface Tenant {
  tenantId: string;
  tenantName: string;
  tenantSlug: string;
  subscriptionTier: 'Free' | 'Basic' | 'Pro' | 'Enterprise';
  isolationStrategy: 'Schema' | 'Database' | 'RowLevel' | 'Hybrid';
  lifecycleState: 'Active' | 'Suspended' | 'Deleted';
  createdAt: Date;
  usageMetrics?: {
    apiCalls: number;
    storage: number;
    users: number;
  };
}

interface TenantProvisionRequest {
  tenantName: string;
  tenantSlug: string;
  subscriptionTier: string;
  isolationStrategy: string;
  adminEmail: string;
}

// API client for tenant management
class TenantManagementAPI {
  private baseUrl: string;

  constructor(baseUrl: string = '/api/tenants') {
    this.baseUrl = baseUrl;
  }

  async listTenants(): Promise<Tenant[]> {
    const response = await fetch(`${this.baseUrl}`);
    if (!response.ok) throw new Error('Failed to fetch tenants');
    return response.json();
  }

  async getTenant(tenantId: string): Promise<Tenant> {
    const response = await fetch(`${this.baseUrl}/${tenantId}`);
    if (!response.ok) throw new Error('Failed to fetch tenant');
    return response.json();
  }

  async provisionTenant(request: TenantProvisionRequest): Promise<Tenant> {
    const response = await fetch(`${this.baseUrl}/provision`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(request),
    });
    if (!response.ok) throw new Error('Failed to provision tenant');
    return response.json();
  }

  async suspendTenant(tenantId: string, reason: string): Promise<void> {
    const response = await fetch(`${this.baseUrl}/${tenantId}/suspend`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ reason }),
    });
    if (!response.ok) throw new Error('Failed to suspend tenant');
  }

  async getUsageMetrics(tenantId: string): Promise<any> {
    const response = await fetch(`${this.baseUrl}/${tenantId}/usage`);
    if (!response.ok) throw new Error('Failed to fetch usage metrics');
    return response.json();
  }
}

// React component: Tenant List Dashboard
export const TenantListDashboard: React.FC = () => {
  const [tenants, setTenants] = useState<Tenant[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const api = new TenantManagementAPI();

  useEffect(() => {
    loadTenants();
  }, []);

  const loadTenants = async () => {
    try {
      setLoading(true);
      const data = await api.listTenants();
      setTenants(data);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Unknown error');
    } finally {
      setLoading(false);
    }
  };

  const handleSuspend = async (tenantId: string) => {
    if (!confirm('Are you sure you want to suspend this tenant?')) return;

    try {
      await api.suspendTenant(tenantId, 'Admin action');
      await loadTenants();
    } catch (err) {
      alert('Failed to suspend tenant');
    }
  };

  if (loading) return <div className="loading">Loading tenants...</div>;
  if (error) return <div className="error">Error: {error}</div>;

  return (
    <div className="tenant-dashboard">
      <h1>Multi-Tenant Admin Dashboard</h1>

      <div className="tenant-stats">
        <div className="stat-card">
          <h3>Total Tenants</h3>
          <p>{tenants.length}</p>
        </div>
        <div className="stat-card">
          <h3>Active</h3>
          <p>{tenants.filter(t => t.lifecycleState === 'Active').length}</p>
        </div>
        <div className="stat-card">
          <h3>Suspended</h3>
          <p>{tenants.filter(t => t.lifecycleState === 'Suspended').length}</p>
        </div>
      </div>

      <table className="tenant-table">
        <thead>
          <tr>
            <th>Tenant Name</th>
            <th>Slug</th>
            <th>Tier</th>
            <th>Isolation</th>
            <th>Status</th>
            <th>Created</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
          {tenants.map(tenant => (
            <tr key={tenant.tenantId}>
              <td>{tenant.tenantName}</td>
              <td>{tenant.tenantSlug}</td>
              <td>
                <span className={`badge tier-${tenant.subscriptionTier.toLowerCase()}`}>
                  {tenant.subscriptionTier}
                </span>
              </td>
              <td>{tenant.isolationStrategy}</td>
              <td>
                <span className={`status ${tenant.lifecycleState.toLowerCase()}`}>
                  {tenant.lifecycleState}
                </span>
              </td>
              <td>{new Date(tenant.createdAt).toLocaleDateString()}</td>
              <td>
                <button onClick={() => handleSuspend(tenant.tenantId)}>
                  Suspend
                </button>
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

// React component: Tenant Provisioning Form
export const TenantProvisioningForm: React.FC<{
  onSuccess: () => void;
}> = ({ onSuccess }) => {
  const [formData, setFormData] = useState<TenantProvisionRequest>({
    tenantName: '',
    tenantSlug: '',
    subscriptionTier: 'Basic',
    isolationStrategy: 'Schema',
    adminEmail: '',
  });

  const api = new TenantManagementAPI();

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    try {
      await api.provisionTenant(formData);
      alert('Tenant provisioned successfully!');
      onSuccess();
    } catch (err) {
      alert('Failed to provision tenant');
    }
  };

  return (
    <form onSubmit={handleSubmit} className="provision-form">
      <h2>Provision New Tenant</h2>

      <div className="form-group">
        <label>Tenant Name</label>
        <input
          type="text"
          value={formData.tenantName}
          onChange={e => setFormData({ ...formData, tenantName: e.target.value })}
          required
        />
      </div>

      <div className="form-group">
        <label>Tenant Slug (subdomain)</label>
        <input
          type="text"
          value={formData.tenantSlug}
          onChange={e => setFormData({ ...formData, tenantSlug: e.target.value })}
          pattern="[a-z0-9-]+"
          required
        />
      </div>

      <div className="form-group">
        <label>Subscription Tier</label>
        <select
          value={formData.subscriptionTier}
          onChange={e => setFormData({ ...formData, subscriptionTier: e.target.value })}
        >
          <option value="Free">Free</option>
          <option value="Basic">Basic</option>
          <option value="Pro">Pro</option>
          <option value="Enterprise">Enterprise</option>
        </select>
      </div>

      <div className="form-group">
        <label>Isolation Strategy</label>
        <select
          value={formData.isolationStrategy}
          onChange={e => setFormData({ ...formData, isolationStrategy: e.target.value })}
        >
          <option value="RowLevel">Row-Level (Shared Schema)</option>
          <option value="Schema">Schema-Level</option>
          <option value="Database">Database-Level</option>
          <option value="Hybrid">Hybrid</option>
        </select>
      </div>

      <div className="form-group">
        <label>Admin Email</label>
        <input
          type="email"
          value={formData.adminEmail}
          onChange={e => setFormData({ ...formData, adminEmail: e.target.value })}
          required
        />
      </div>

      <button type="submit">Provision Tenant</button>
    </form>
  );
};

export default TenantListDashboard;
