import { create } from 'zustand'
import { persist } from 'zustand/middleware'
import { User, Organization } from '@/types'
import { idpClient } from '@/api/client'

interface AuthState {
  user: User | null
  organization: Organization | null
  accessToken: string | null
  refreshToken: string | null
  isAuthenticated: boolean
  isLoading: boolean
  error: string | null

  // Actions
  login: (orgId: string, username: string, password: string) => Promise<void>
  logout: () => void
  setUser: (user: User) => void
  setOrganization: (org: Organization) => void
  clearError: () => void
}

export const useAuthStore = create<AuthState>()(
  persist(
    (set) => ({
      user: null,
      organization: null,
      accessToken: null,
      refreshToken: null,
      isAuthenticated: false,
      isLoading: false,
      error: null,

      login: async (orgId: string, username: string, password: string) => {
        set({ isLoading: true, error: null })
        try {
          const response = await idpClient.login(orgId, { username, password })
          if (response.data?.accessToken) {
            set({
              accessToken: response.data.accessToken,
              refreshToken: response.data.refreshToken,
              isAuthenticated: true,
              isLoading: false,
            })
          }
        } catch (error) {
          set({
            error: error instanceof Error ? error.message : 'Login failed',
            isLoading: false,
          })
          throw error
        }
      },

      logout: () => {
        set({
          user: null,
          organization: null,
          accessToken: null,
          refreshToken: null,
          isAuthenticated: false,
        })
      },

      setUser: (user: User) => {
        set({ user })
      },

      setOrganization: (organization: Organization) => {
        set({ organization })
      },

      clearError: () => {
        set({ error: null })
      },
    }),
    {
      name: 'auth-store',
      partialize: (state) => ({
        user: state.user,
        organization: state.organization,
        accessToken: state.accessToken,
        refreshToken: state.refreshToken,
        isAuthenticated: state.isAuthenticated,
      }),
    }
  )
)
