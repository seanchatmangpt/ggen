import { create } from 'zustand'
import type { User, Team, Service, Template, Notification } from '@/types'

interface IDPState {
  // User & Auth
  user: User | null
  isAuthenticated: boolean
  setUser: (user: User | null) => void

  // Current Context
  currentTeam: Team | null
  setCurrentTeam: (team: Team | null) => void

  // Data
  services: Service[]
  teams: Team[]
  templates: Template[]
  notifications: Notification[]

  // Actions
  setServices: (services: Service[]) => void
  setTeams: (teams: Team[]) => void
  setTemplates: (templates: Template[]) => void
  addNotification: (notification: Notification) => void
  removeNotification: (id: string) => void

  // UI State
  sidebarOpen: boolean
  setSidebarOpen: (open: boolean) => void
  darkMode: boolean
  setDarkMode: (dark: boolean) => void
}

export const useIDPStore = create<IDPState>((set) => ({
  // Initial state
  user: null,
  isAuthenticated: false,
  currentTeam: null,
  services: [],
  teams: [],
  templates: [],
  notifications: [],
  sidebarOpen: true,
  darkMode: false,

  // Actions
  setUser: (user) =>
    set({
      user,
      isAuthenticated: !!user,
    }),

  setCurrentTeam: (team) => set({ currentTeam: team }),

  setServices: (services) => set({ services }),

  setTeams: (teams) => set({ teams }),

  setTemplates: (templates) => set({ templates }),

  addNotification: (notification) =>
    set((state) => ({
      notifications: [notification, ...state.notifications],
    })),

  removeNotification: (id) =>
    set((state) => ({
      notifications: state.notifications.filter((n) => n.id !== id),
    })),

  setSidebarOpen: (open) => set({ sidebarOpen: open }),

  setDarkMode: (dark) => set({ darkMode: dark }),
}))
