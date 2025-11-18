import axios, { AxiosInstance, AxiosError } from 'axios'
import {
  User,
  Organization,
  Role,
  AuthFlow,
  OAuth2Client,
  AuditLogEntry,
  ApiResponse,
  PaginatedResponse,
} from '@/types'

const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000'

class IdpApiClient {
  private client: AxiosInstance

  constructor() {
    this.client = axios.create({
      baseURL: API_URL,
      headers: {
        'Content-Type': 'application/json',
      },
    })

    // Add token to requests
    this.client.interceptors.request.use((config) => {
      const token = typeof window !== 'undefined' ? localStorage.getItem('access_token') : null
      if (token) {
        config.headers.Authorization = `Bearer ${token}`
      }
      return config
    })

    // Handle errors
    this.client.interceptors.response.use(
      (response) => response,
      (error: AxiosError) => {
        if (error.response?.status === 401) {
          // Handle unauthorized
          typeof window !== 'undefined' && localStorage.removeItem('access_token')
          typeof window !== 'undefined' && window.location.href = '/login'
        }
        return Promise.reject(error)
      }
    )
  }

  // Authentication
  async register(orgId: string, data: { username: string; email: string; password: string }) {
    const response = await this.client.post<ApiResponse<User>>(`/auth/${orgId}/register`, data)
    return response.data
  }

  async login(orgId: string, data: { username: string; password: string }) {
    const response = await this.client.post<ApiResponse<{ accessToken: string; refreshToken: string }>>(
      `/auth/${orgId}/login`,
      data
    )
    return response.data
  }

  async refreshToken(refreshToken: string) {
    const response = await this.client.post<ApiResponse<{ accessToken: string }>>(`/auth/refresh`, {
      refresh_token: refreshToken,
    })
    return response.data
  }

  // Users
  async getUser(userId: string) {
    const response = await this.client.get<ApiResponse<User>>(`/users/${userId}`)
    return response.data.data
  }

  async listUsers(orgId: string, page = 1, pageSize = 20) {
    const response = await this.client.get<ApiResponse<PaginatedResponse<User>>>(
      `/orgs/${orgId}/users`,
      { params: { page, page_size: pageSize } }
    )
    return response.data.data
  }

  async updateUser(userId: string, data: Partial<User>) {
    const response = await this.client.put<ApiResponse<User>>(`/users/${userId}`, data)
    return response.data.data
  }

  async deleteUser(userId: string) {
    await this.client.delete(`/users/${userId}`)
  }

  // Organizations
  async getOrganization(orgId: string) {
    const response = await this.client.get<ApiResponse<Organization>>(`/orgs/${orgId}`)
    return response.data.data
  }

  async createOrganization(data: Partial<Organization>) {
    const response = await this.client.post<ApiResponse<Organization>>(`/orgs`, data)
    return response.data.data
  }

  async updateOrganization(orgId: string, data: Partial<Organization>) {
    const response = await this.client.put<ApiResponse<Organization>>(`/orgs/${orgId}`, data)
    return response.data.data
  }

  // Roles & RBAC
  async listRoles(orgId: string) {
    const response = await this.client.get<ApiResponse<Role[]>>(`/orgs/${orgId}/roles`)
    return response.data.data || []
  }

  async createRole(orgId: string, data: Partial<Role>) {
    const response = await this.client.post<ApiResponse<Role>>(`/orgs/${orgId}/roles`, data)
    return response.data.data
  }

  async updateRole(roleId: string, data: Partial<Role>) {
    const response = await this.client.put<ApiResponse<Role>>(`/roles/${roleId}`, data)
    return response.data.data
  }

  async deleteRole(roleId: string) {
    await this.client.delete(`/roles/${roleId}`)
  }

  async assignRole(userId: string, orgId: string, roleId: string) {
    const response = await this.client.post(`/rbac/assign/${userId}/${orgId}`, { role_id: roleId })
    return response.data
  }

  async checkPermission(userId: string, resource: string, action: string) {
    const response = await this.client.post<ApiResponse<{ allowed: boolean; reason?: string }>>(
      `/rbac/check/${userId}`,
      { resource, action }
    )
    return response.data.data?.allowed || false
  }

  // Auth Flows
  async listAuthFlows(orgId: string) {
    const response = await this.client.get<ApiResponse<AuthFlow[]>>(`/orgs/${orgId}/auth-flows`)
    return response.data.data || []
  }

  async getAuthFlow(flowId: string) {
    const response = await this.client.get<ApiResponse<AuthFlow>>(`/auth-flows/${flowId}`)
    return response.data.data
  }

  async createAuthFlow(orgId: string, data: Partial<AuthFlow>) {
    const response = await this.client.post<ApiResponse<AuthFlow>>(`/orgs/${orgId}/auth-flows`, data)
    return response.data.data
  }

  async updateAuthFlow(flowId: string, data: Partial<AuthFlow>) {
    const response = await this.client.put<ApiResponse<AuthFlow>>(`/auth-flows/${flowId}`, data)
    return response.data.data
  }

  async deleteAuthFlow(flowId: string) {
    await this.client.delete(`/auth-flows/${flowId}`)
  }

  async executeAuthFlow(flowId: string, context: Record<string, any>) {
    const response = await this.client.post(`/auth-flows/${flowId}/execute`, { context })
    return response.data
  }

  // OAuth2 Clients
  async listOAuth2Clients(orgId: string) {
    const response = await this.client.get<ApiResponse<OAuth2Client[]>>(
      `/orgs/${orgId}/oauth2-clients`
    )
    return response.data.data || []
  }

  async createOAuth2Client(orgId: string, data: Partial<OAuth2Client>) {
    const response = await this.client.post<ApiResponse<OAuth2Client>>(
      `/orgs/${orgId}/oauth2-clients`,
      data
    )
    return response.data.data
  }

  async updateOAuth2Client(clientId: string, data: Partial<OAuth2Client>) {
    const response = await this.client.put<ApiResponse<OAuth2Client>>(
      `/oauth2-clients/${clientId}`,
      data
    )
    return response.data.data
  }

  async deleteOAuth2Client(clientId: string) {
    await this.client.delete(`/oauth2-clients/${clientId}`)
  }

  async regenerateOAuth2ClientSecret(clientId: string) {
    const response = await this.client.post<ApiResponse<OAuth2Client>>(
      `/oauth2-clients/${clientId}/regenerate-secret`
    )
    return response.data.data
  }

  // Audit Logs
  async getAuditLogs(orgId: string, page = 1, pageSize = 20) {
    const response = await this.client.get<ApiResponse<PaginatedResponse<AuditLogEntry>>>(
      `/audit/org/${orgId}`,
      { params: { page, page_size: pageSize } }
    )
    return response.data.data
  }

  async getUserAuditLogs(userId: string, page = 1, pageSize = 20) {
    const response = await this.client.get<ApiResponse<PaginatedResponse<AuditLogEntry>>>(
      `/audit/user/${userId}`,
      { params: { page, page_size: pageSize } }
    )
    return response.data.data
  }

  // Health Check
  async health() {
    try {
      const response = await this.client.get('/health')
      return response.status === 200
    } catch {
      return false
    }
  }
}

export const idpClient = new IdpApiClient()
