import Link from "next/link"
import { Card } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import { Badge } from "@/components/ui/badge"

export default function Home() {
  return (
    <div className="space-y-8">
      {/* Header */}
      <div className="space-y-2">
        <h1 className="text-4xl font-bold tracking-tight">YAWL Workflow Editor</h1>
        <p className="text-lg text-slate-600">
          Next.js + shadcn/ui powered workflow management system with SPARQL integration
        </p>
      </div>

      {/* Stats */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <Card className="p-6">
          <div className="space-y-2">
            <p className="text-sm font-medium text-slate-600">Total Cases</p>
            <p className="text-3xl font-bold">-</p>
            <p className="text-xs text-slate-500">Loading from SPARQL...</p>
          </div>
        </Card>
        <Card className="p-6">
          <div className="space-y-2">
            <p className="text-sm font-medium text-slate-600">Active Workitems</p>
            <p className="text-3xl font-bold">-</p>
            <p className="text-xs text-slate-500">Loading from SPARQL...</p>
          </div>
        </Card>
        <Card className="p-6">
          <div className="space-y-2">
            <p className="text-sm font-medium text-slate-600">Running Processes</p>
            <p className="text-3xl font-bold">-</p>
            <p className="text-xs text-slate-500">Loading from SPARQL...</p>
          </div>
        </Card>
        <Card className="p-6">
          <div className="space-y-2">
            <p className="text-sm font-medium text-slate-600">Resources</p>
            <p className="text-3xl font-bold">-</p>
            <p className="text-xs text-slate-500">Loading from SPARQL...</p>
          </div>
        </Card>
      </div>

      {/* Quick Actions */}
      <div className="space-y-4">
        <h2 className="text-2xl font-bold">Quick Actions</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <Card className="p-6 space-y-4">
            <div className="space-y-2">
              <h3 className="text-lg font-semibold">Manage Cases</h3>
              <p className="text-sm text-slate-600">
                View, create, and manage workflow case instances
              </p>
            </div>
            <Link href="/cases">
              <Button className="w-full">Go to Cases</Button>
            </Link>
          </Card>

          <Card className="p-6 space-y-4">
            <div className="space-y-2">
              <h3 className="text-lg font-semibold">View Workitems</h3>
              <p className="text-sm text-slate-600">
                Monitor tasks assigned to users and resources
              </p>
            </div>
            <Link href="/workitems">
              <Button className="w-full">Go to Workitems</Button>
            </Link>
          </Card>

          <Card className="p-6 space-y-4">
            <div className="space-y-2">
              <h3 className="text-lg font-semibold">Manage Processes</h3>
              <p className="text-sm text-slate-600">
                Define and configure workflow processes
              </p>
            </div>
            <Link href="/processes">
              <Button className="w-full">Go to Processes</Button>
            </Link>
          </Card>

          <Card className="p-6 space-y-4">
            <div className="space-y-2">
              <h3 className="text-lg font-semibold">Resource Management</h3>
              <p className="text-sm text-slate-600">
                Manage users, roles, and resource allocation
              </p>
            </div>
            <Link href="/resources">
              <Button className="w-full">Go to Resources</Button>
            </Link>
          </Card>
        </div>
      </div>

      {/* Technology Stack */}
      <div className="space-y-4">
        <h2 className="text-2xl font-bold">Technology Stack</h2>
        <Card className="p-6">
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            <div className="space-y-2">
              <Badge variant="secondary">Next.js 14</Badge>
              <p className="text-xs text-slate-600">React Framework</p>
            </div>
            <div className="space-y-2">
              <Badge variant="secondary">shadcn/ui</Badge>
              <p className="text-xs text-slate-600">UI Components</p>
            </div>
            <div className="space-y-2">
              <Badge variant="secondary">SPARQL</Badge>
              <p className="text-xs text-slate-600">Data Queries</p>
            </div>
            <div className="space-y-2">
              <Badge variant="secondary">RDF/OWL</Badge>
              <p className="text-xs text-slate-600">Data Model</p>
            </div>
            <div className="space-y-2">
              <Badge variant="secondary">Zod</Badge>
              <p className="text-xs text-slate-600">Validation</p>
            </div>
            <div className="space-y-2">
              <Badge variant="secondary">TypeScript</Badge>
              <p className="text-xs text-slate-600">Type Safety</p>
            </div>
            <div className="space-y-2">
              <Badge variant="secondary">Tailwind CSS</Badge>
              <p className="text-xs text-slate-600">Styling</p>
            </div>
            <div className="space-y-2">
              <Badge variant="secondary">YAWL 5.0</Badge>
              <p className="text-xs text-slate-600">Workflow Engine</p>
            </div>
          </div>
        </Card>
      </div>

      {/* Features */}
      <div className="space-y-4">
        <h2 className="text-2xl font-bold">Features</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <Card className="p-4">
            <ul className="space-y-2">
              <li className="flex gap-2">
                <span className="text-green-600">✓</span>
                <span className="text-sm">Case management and lifecycle tracking</span>
              </li>
              <li className="flex gap-2">
                <span className="text-green-600">✓</span>
                <span className="text-sm">Workitem allocation and status management</span>
              </li>
              <li className="flex gap-2">
                <span className="text-green-600">✓</span>
                <span className="text-sm">Process definition and task configuration</span>
              </li>
              <li className="flex gap-2">
                <span className="text-green-600">✓</span>
                <span className="text-sm">Resource allocation and role-based assignment</span>
              </li>
            </ul>
          </Card>
          <Card className="p-4">
            <ul className="space-y-2">
              <li className="flex gap-2">
                <span className="text-green-600">✓</span>
                <span className="text-sm">SPARQL query integration for flexible data access</span>
              </li>
              <li className="flex gap-2">
                <span className="text-green-600">✓</span>
                <span className="text-sm">RDF/OWL ontology-driven architecture</span>
              </li>
              <li className="flex gap-2">
                <span className="text-green-600">✓</span>
                <span className="text-span className="text-sm">Type-safe with TypeScript and Zod validation</span>
              </li>
              <li className="flex gap-2">
                <span className="text-green-600">✓</span>
                <span className="text-sm">Responsive UI with shadcn components</span>
              </li>
            </ul>
          </Card>
        </div>
      </div>
    </div>
  )
}
