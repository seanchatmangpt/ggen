import Link from "next/link"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import { FileCode, Workflow, Sparkles, GitBranch } from "lucide-react"

export default function Home() {
  return (
    <div className="space-y-8">
      <div className="text-center space-y-4">
        <h1 className="text-4xl font-bold text-slate-900">
          Ontology-Driven Development
        </h1>
        <p className="text-xl text-slate-600 max-w-2xl mx-auto">
          Modify the ontology, regenerate, and watch your entire app update automatically
        </p>
      </div>

      <div className="grid md:grid-cols-2 gap-6 max-w-4xl mx-auto">
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <FileCode className="h-5 w-5" />
              Tasks
            </CardTitle>
            <CardDescription>
              View and manage tasks - auto-generated from ontology
            </CardDescription>
          </CardHeader>
          <CardContent>
            <Link href="/tasks">
              <Button className="w-full">View Tasks</Button>
            </Link>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Workflow className="h-5 w-5" />
              Projects
            </CardTitle>
            <CardDescription>
              Manage projects - auto-generated from ontology
            </CardDescription>
          </CardHeader>
          <CardContent>
            <Link href="/projects">
              <Button className="w-full">View Projects</Button>
            </Link>
          </CardContent>
        </Card>
      </div>

      <Card className="max-w-4xl mx-auto bg-gradient-to-br from-blue-50 to-indigo-50 border-blue-200">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Sparkles className="h-5 w-5 text-blue-600" />
            How It Works
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="space-y-2">
            <h3 className="font-semibold text-slate-900 flex items-center gap-2">
              <GitBranch className="h-4 w-4" />
              1. Modify the Ontology
            </h3>
            <p className="text-sm text-slate-600 ml-6">
              Edit <code className="bg-white px-2 py-1 rounded">ontology/task-management.ttl</code> to add/modify properties:
            </p>
            <pre className="bg-white p-3 rounded-lg text-xs overflow-x-auto ml-6 border">
{`ex:estimatedHours a owl:DatatypeProperty ;
    rdfs:domain ex:Task ;
    rdfs:range xsd:integer ;
    rdfs:comment "Estimated hours for task" .`}
            </pre>
          </div>

          <div className="space-y-2">
            <h3 className="font-semibold text-slate-900 flex items-center gap-2">
              <Workflow className="h-4 w-4" />
              2. Regenerate
            </h3>
            <p className="text-sm text-slate-600 ml-6">
              Run the regeneration script or let Git hooks do it automatically:
            </p>
            <pre className="bg-white p-3 rounded-lg text-xs ml-6 border">
              npm run regenerate
            </pre>
          </div>

          <div className="space-y-2">
            <h3 className="font-semibold text-slate-900 flex items-center gap-2">
              <Sparkles className="h-4 w-4" />
              3. See the Magic
            </h3>
            <p className="text-sm text-slate-600 ml-6">
              Types, validation schemas, API routes, and UI components are automatically generated
            </p>
            <ul className="text-xs text-slate-500 ml-12 space-y-1 list-disc">
              <li>TypeScript types in <code>lib/types.ts</code></li>
              <li>Zod validation in <code>lib/validation.ts</code></li>
              <li>API routes in <code>app/api/</code></li>
              <li>CRUD tables in <code>components/generated/</code></li>
            </ul>
          </div>
        </CardContent>
      </Card>

      <div className="max-w-4xl mx-auto bg-yellow-50 border border-yellow-200 rounded-lg p-4">
        <p className="text-sm text-yellow-800">
          <strong>💡 Pro Tip:</strong> Git hooks automatically regenerate code when you pull changes to the ontology.
          Your team stays in sync effortlessly!
        </p>
      </div>
    </div>
  )
}
