"use client"

import React, { useState } from "react"
import Link from "next/link"
import { useIDPStore } from "@/hooks"
import {
  Boxes,
  LayoutGrid,
  FileText,
  Users,
  Zap,
  ArrowRight,
  Building2,
  Search,
  Bell,
  Settings,
  LogOut,
  Menu,
  X,
} from "lucide-react"

// Mock data for demo
const MOCK_SERVICES = [
  { id: "1", name: "User Service", description: "User management and authentication", status: "healthy", team: "Platform", type: "backend" },
  { id: "2", name: "API Gateway", description: "Central API gateway", status: "healthy", team: "Platform", type: "tool" },
  { id: "3", name: "Dashboard", description: "Admin dashboard", status: "warning", team: "Frontend", type: "frontend" },
  { id: "4", name: "Search Service", description: "Elasticsearch wrapper", status: "healthy", team: "Infrastructure", type: "backend" },
]

const MOCK_TEMPLATES = [
  { id: "1", name: "Node.js Backend", language: "TypeScript", rating: 4.8, downloads: 1250 },
  { id: "2", name: "React Frontend", language: "TypeScript", rating: 4.9, downloads: 2150 },
  { id: "3", name: "Python FastAPI", language: "Python", rating: 4.6, downloads: 890 },
]

const MOCK_TEAMS = [
  { id: "1", name: "Platform Team", members: 8 },
  { id: "2", name: "Frontend Team", members: 6 },
  { id: "3", name: "Infrastructure", members: 5 },
]

export default function Home() {
  const [sidebarOpen, setSidebarOpen] = useState(true)
  const { user, setUser } = useIDPStore()

  // Set demo user
  React.useEffect(() => {
    if (!user) {
      setUser({
        id: "user-1",
        email: "dev@company.com",
        name: "John Developer",
        roles: ["member"],
        teams: [],
        createdAt: new Date(),
      })
    }
  }, [user, setUser])

  return (
    <div className="flex h-screen bg-background">
      {/* Sidebar */}
      <aside
        className={`${
          sidebarOpen ? "w-64" : "w-20"
        } border-r border-border bg-card transition-all duration-300 flex flex-col`}
      >
        {/* Logo */}
        <div className="flex items-center justify-between p-6 border-b border-border">
          <div className={`flex items-center gap-3 ${!sidebarOpen && "flex-col"}`}>
            <div className="h-8 w-8 rounded-lg bg-primary flex items-center justify-center">
              <Boxes className="h-5 w-5 text-primary-foreground" />
            </div>
            {sidebarOpen && <span className="font-bold text-lg">Backstage</span>}
          </div>
          <button
            onClick={() => setSidebarOpen(!sidebarOpen)}
            className="p-1 hover:bg-muted rounded"
          >
            {sidebarOpen ? (
              <X className="h-4 w-4" />
            ) : (
              <Menu className="h-4 w-4" />
            )}
          </button>
        </div>

        {/* Navigation */}
        <nav className="flex-1 p-4 space-y-2">
          <NavLink icon={<LayoutGrid className="h-5 w-5" />} label="Dashboard" href="/" active sidebarOpen={sidebarOpen} />
          <NavLink icon={<Boxes className="h-5 w-5" />} label="Catalog" href="/catalog" sidebarOpen={sidebarOpen} />
          <NavLink icon={<Zap className="h-5 w-5" />} label="Templates" href="/templates" sidebarOpen={sidebarOpen} />
          <NavLink icon={<FileText className="h-5 w-5" />} label="Documentation" href="/docs" sidebarOpen={sidebarOpen} />
          <NavLink icon={<Users className="h-5 w-5" />} label="Teams" href="/teams" sidebarOpen={sidebarOpen} />
        </nav>

        {/* User Section */}
        <div className="border-t border-border p-4 space-y-2">
          <NavLink icon={<Settings className="h-5 w-5" />} label="Settings" href="/settings" sidebarOpen={sidebarOpen} />
          <NavLink icon={<LogOut className="h-5 w-5" />} label="Logout" href="/logout" sidebarOpen={sidebarOpen} />
        </div>
      </aside>

      {/* Main Content */}
      <div className="flex-1 flex flex-col overflow-hidden">
        {/* Top Header */}
        <header className="idp-header flex items-center justify-between">
          <div className="flex items-center gap-4 flex-1">
            <div className="relative flex-1 max-w-md">
              <Search className="absolute left-3 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground" />
              <input
                type="text"
                placeholder="Search services, teams..."
                className="w-full pl-10 pr-4 py-2 rounded-lg border border-border bg-background focus:outline-none focus:ring-2 focus:ring-primary"
              />
            </div>
          </div>

          <div className="flex items-center gap-4">
            <button className="p-2 hover:bg-muted rounded-lg relative">
              <Bell className="h-5 w-5" />
              <span className="absolute top-1 right-1 h-2 w-2 bg-red-500 rounded-full" />
            </button>
            <div className="flex items-center gap-3 pl-4 border-l border-border">
              <div className="text-right">
                <div className="text-sm font-medium">{user?.name || "User"}</div>
                <div className="text-xs text-muted-foreground">{user?.email}</div>
              </div>
              <div className="h-8 w-8 rounded-full bg-primary text-primary-foreground flex items-center justify-center text-sm font-bold">
                {user?.name?.charAt(0)}
              </div>
            </div>
          </div>
        </header>

        {/* Page Content */}
        <main className="flex-1 overflow-auto p-6 space-y-6">
          {/* Welcome Section */}
          <div>
            <h1 className="text-3xl font-bold mb-2">Welcome back, {user?.name?.split(" ")[0]}!</h1>
            <p className="text-muted-foreground">Here&apos;s a quick overview of your development platform</p>
          </div>

          {/* Stats Grid */}
          <div className="grid grid-cols-4 gap-4">
            <StatsCard label="Services" value={MOCK_SERVICES.length} icon={<Boxes className="h-6 w-6" />} trend="+12%" />
            <StatsCard label="Templates" value={MOCK_TEMPLATES.length} icon={<Zap className="h-6 w-6" />} trend="+5%" />
            <StatsCard label="Teams" value={MOCK_TEAMS.length} icon={<Users className="h-6 w-6" />} trend="+2%" />
            <StatsCard label="Healthy Services" value={`${MOCK_SERVICES.filter(s => s.status === "healthy").length}/4`} icon={<Building2 className="h-6 w-6" />} trend="100%" />
          </div>

          <div className="grid grid-cols-3 gap-6">
            {/* Services Section */}
            <div className="idp-card col-span-2">
              <div className="flex items-center justify-between mb-4">
                <h2 className="text-xl font-bold">Recent Services</h2>
                <Link href="/catalog" className="text-sm text-primary hover:underline flex items-center gap-1">
                  View All <ArrowRight className="h-4 w-4" />
                </Link>
              </div>
              <div className="space-y-3">
                {MOCK_SERVICES.slice(0, 3).map((service) => (
                  <ServiceRow key={service.id} service={service} />
                ))}
              </div>
            </div>

            {/* Quick Actions */}
            <div className="idp-card">
              <h2 className="text-xl font-bold mb-4">Quick Actions</h2>
              <div className="space-y-2">
                <ActionButton label="Create Service" icon={<Boxes className="h-4 w-4" />} />
                <ActionButton label="Create Template" icon={<Zap className="h-4 w-4" />} />
                <ActionButton label="Create Team" icon={<Users className="h-4 w-4" />} />
                <ActionButton label="View Documentation" icon={<FileText className="h-4 w-4" />} />
              </div>
            </div>
          </div>

          {/* Templates Section */}
          <div className="idp-card">
            <div className="flex items-center justify-between mb-4">
              <h2 className="text-xl font-bold">Popular Templates</h2>
              <Link href="/templates" className="text-sm text-primary hover:underline flex items-center gap-1">
                Browse All <ArrowRight className="h-4 w-4" />
              </Link>
            </div>
            <div className="grid grid-cols-3 gap-4">
              {MOCK_TEMPLATES.map((template) => (
                <TemplateCard key={template.id} template={template} />
              ))}
            </div>
          </div>
        </main>
      </div>
    </div>
  )
}

// Component: Navigation Link
function NavLink({ icon, label, href, active, sidebarOpen }: any) {
  return (
    <Link href={href} className={`idp-sidebar-item ${active ? "active" : ""}`}>
      {icon}
      {sidebarOpen && <span>{label}</span>}
    </Link>
  )
}

// Component: Stats Card
function StatsCard({ label, value, icon, trend }: any) {
  return (
    <div className="idp-card">
      <div className="flex items-center justify-between">
        <div>
          <p className="text-sm text-muted-foreground">{label}</p>
          <p className="text-2xl font-bold mt-1">{value}</p>
          <p className="text-xs text-green-600 mt-1">{trend} this month</p>
        </div>
        <div className="h-12 w-12 rounded-lg bg-primary/10 flex items-center justify-center text-primary">
          {icon}
        </div>
      </div>
    </div>
  )
}

// Component: Service Row
function ServiceRow({ service }: any) {
  const statusColor = {
    healthy: "bg-green-100 text-green-800",
    warning: "bg-yellow-100 text-yellow-800",
    critical: "bg-red-100 text-red-800",
  }

  return (
    <div className="flex items-center justify-between p-3 border border-border rounded-lg hover:bg-muted transition-colors">
      <div className="flex-1">
        <p className="font-medium">{service.name}</p>
        <p className="text-xs text-muted-foreground">{service.description}</p>
      </div>
      <div className="flex items-center gap-3">
        <span className="text-xs px-2 py-1 bg-secondary rounded">{service.team}</span>
        <span className={`text-xs px-2 py-1 rounded ${statusColor[service.status as keyof typeof statusColor]}`}>
          {service.status}
        </span>
      </div>
    </div>
  )
}

// Component: Template Card
function TemplateCard({ template }: any) {
  return (
    <div className="border border-border rounded-lg p-4 hover:border-primary transition-colors cursor-pointer">
      <div className="h-20 bg-gradient-to-br from-primary/10 to-secondary/10 rounded mb-3" />
      <p className="font-medium truncate">{template.name}</p>
      <p className="text-xs text-muted-foreground">{template.language}</p>
      <div className="flex items-center justify-between mt-3 pt-3 border-t border-border">
        <div className="flex items-center gap-1">
          <span className="text-xs">⭐ {template.rating}</span>
        </div>
        <span className="text-xs text-muted-foreground">{template.downloads} uses</span>
      </div>
    </div>
  )
}

// Component: Action Button
function ActionButton({ label, icon }: any) {
  return (
    <button className="w-full flex items-center gap-2 p-2 rounded-lg border border-border hover:bg-muted transition-colors text-sm">
      {icon}
      {label}
    </button>
  )
}
