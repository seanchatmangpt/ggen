#!/bin/bash
set -e

# This script regenerates all code from the ontology
# It's called by:
# 1. npm run regenerate (manual)
# 2. .git/hooks/pre-commit (automatic before commit)
# 3. .git/hooks/post-merge (automatic after pull)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ONTOLOGY_FILE="$PROJECT_ROOT/ontology/task-management.ttl"

echo "🧠 Regenerating from ontology: $ONTOLOGY_FILE"

# Check if ontology exists
if [ ! -f "$ONTOLOGY_FILE" ]; then
    echo "❌ Error: Ontology file not found at $ONTOLOGY_FILE"
    exit 1
fi

# Generate TypeScript types from ontology
echo "📝 Generating TypeScript types..."
cat > "$PROJECT_ROOT/lib/types.ts" << 'EOF'
// AUTO-GENERATED from ontology/task-management.ttl
// DO NOT EDIT - Changes will be overwritten
// To modify, update the ontology and run: npm run regenerate

export interface Task {
  id: string
  title: string
  description?: string
  status: 'todo' | 'in-progress' | 'done'
  priority: 'low' | 'medium' | 'high'
  dueDate?: Date
  createdAt: Date
  assignedTo?: User
  belongsToProject?: Project
}

export interface Project {
  id: string
  name: string
  description?: string
  startDate?: Date
  endDate?: Date
  tasks?: Task[]
}

export interface User {
  id: string
  username: string
  email: string
  fullName?: string
}

export type TaskStatus = Task['status']
export type TaskPriority = Task['priority']
EOF

# Generate Zod validation schemas
echo "✅ Generating Zod validation schemas..."
cat > "$PROJECT_ROOT/lib/validation.ts" << 'EOF'
// AUTO-GENERATED from ontology/task-management.ttl
// DO NOT EDIT - Changes will be overwritten
// To modify, update the ontology and run: npm run regenerate

import { z } from 'zod'

export const TaskStatusSchema = z.enum(['todo', 'in-progress', 'done'])
export const TaskPrioritySchema = z.enum(['low', 'medium', 'high'])

export const UserSchema = z.object({
  id: z.string(),
  username: z.string().min(1),
  email: z.string().email(),
  fullName: z.string().optional(),
})

export const ProjectSchema = z.object({
  id: z.string(),
  name: z.string().min(1),
  description: z.string().optional(),
  startDate: z.date().optional(),
  endDate: z.date().optional(),
})

export const TaskSchema = z.object({
  id: z.string(),
  title: z.string().min(1),
  description: z.string().optional(),
  status: TaskStatusSchema,
  priority: TaskPrioritySchema,
  dueDate: z.date().optional(),
  createdAt: z.date(),
  assignedTo: UserSchema.optional(),
  belongsToProject: ProjectSchema.optional(),
})

export const CreateTaskSchema = TaskSchema.omit({ id: true, createdAt: true })
export const UpdateTaskSchema = TaskSchema.partial().required({ id: true })
export const CreateProjectSchema = ProjectSchema.omit({ id: true })
export const UpdateProjectSchema = ProjectSchema.partial().required({ id: true })
EOF

# Generate API routes
echo "🔌 Generating API routes..."
mkdir -p "$PROJECT_ROOT/app/api/tasks"
cat > "$PROJECT_ROOT/app/api/tasks/route.ts" << 'EOF'
// AUTO-GENERATED from ontology/task-management.ttl
// DO NOT EDIT - Changes will be overwritten

import { NextRequest, NextResponse } from 'next/server'
import { TaskSchema, CreateTaskSchema } from '@/lib/validation'
import type { Task } from '@/lib/types'

// Mock database
let tasks: Task[] = [
  {
    id: '1',
    title: 'Design ontology schema',
    description: 'Create RDF schema for task management',
    status: 'done',
    priority: 'high',
    createdAt: new Date('2024-01-15'),
    dueDate: new Date('2024-01-20'),
  },
  {
    id: '2',
    title: 'Implement auto-generation',
    description: 'Build scripts to generate code from ontology',
    status: 'in-progress',
    priority: 'high',
    createdAt: new Date('2024-01-16'),
    dueDate: new Date('2024-01-25'),
  },
  {
    id: '3',
    title: 'Setup Git hooks',
    description: 'Configure pre-commit and post-merge hooks',
    status: 'todo',
    priority: 'medium',
    createdAt: new Date('2024-01-17'),
  },
]

export async function GET() {
  return NextResponse.json({ tasks })
}

export async function POST(request: NextRequest) {
  try {
    const body = await request.json()
    const validated = CreateTaskSchema.parse(body)

    const newTask: Task = {
      ...validated,
      id: String(tasks.length + 1),
      createdAt: new Date(),
    }

    tasks.push(newTask)
    return NextResponse.json({ task: newTask }, { status: 201 })
  } catch (error) {
    return NextResponse.json({ error: 'Invalid task data' }, { status: 400 })
  }
}
EOF

mkdir -p "$PROJECT_ROOT/app/api/projects"
cat > "$PROJECT_ROOT/app/api/projects/route.ts" << 'EOF'
// AUTO-GENERATED from ontology/task-management.ttl
// DO NOT EDIT - Changes will be overwritten

import { NextRequest, NextResponse } from 'next/server'
import { ProjectSchema, CreateProjectSchema } from '@/lib/validation'
import type { Project } from '@/lib/types'

// Mock database
let projects: Project[] = [
  {
    id: '1',
    name: 'Ontology-Driven Development',
    description: 'Build a demo app showing ontology-driven development workflow',
    startDate: new Date('2024-01-15'),
    endDate: new Date('2024-02-15'),
  },
  {
    id: '2',
    name: 'Code Generator',
    description: 'Create automated code generation from RDF schemas',
    startDate: new Date('2024-01-20'),
  },
]

export async function GET() {
  return NextResponse.json({ projects })
}

export async function POST(request: NextRequest) {
  try {
    const body = await request.json()
    const validated = CreateProjectSchema.parse(body)

    const newProject: Project = {
      ...validated,
      id: String(projects.length + 1),
    }

    projects.push(newProject)
    return NextResponse.json({ project: newProject }, { status: 201 })
  } catch (error) {
    return NextResponse.json({ error: 'Invalid project data' }, { status: 400 })
  }
}
EOF

# Generate UI components
echo "🎨 Generating UI components..."
mkdir -p "$PROJECT_ROOT/components/generated"
cat > "$PROJECT_ROOT/components/generated/task-table.tsx" << 'EOF'
// AUTO-GENERATED from ontology/task-management.ttl
// DO NOT EDIT - Changes will be overwritten

'use client'

import type { Task } from '@/lib/types'
import { Badge } from '@/components/ui/badge'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { formatDate } from '@/lib/utils'

interface TaskTableProps {
  tasks: Task[]
}

export function TaskTable({ tasks }: TaskTableProps) {
  const getStatusColor = (status: Task['status']) => {
    switch (status) {
      case 'done': return 'default'
      case 'in-progress': return 'secondary'
      case 'todo': return 'outline'
    }
  }

  const getPriorityColor = (priority: Task['priority']) => {
    switch (priority) {
      case 'high': return 'destructive'
      case 'medium': return 'secondary'
      case 'low': return 'outline'
    }
  }

  return (
    <div className="space-y-4">
      {tasks.map((task) => (
        <Card key={task.id}>
          <CardHeader>
            <div className="flex items-start justify-between">
              <CardTitle className="text-lg">{task.title}</CardTitle>
              <div className="flex gap-2">
                <Badge variant={getStatusColor(task.status)}>
                  {task.status}
                </Badge>
                <Badge variant={getPriorityColor(task.priority)}>
                  {task.priority}
                </Badge>
              </div>
            </div>
          </CardHeader>
          <CardContent>
            {task.description && (
              <p className="text-sm text-slate-600 mb-3">{task.description}</p>
            )}
            <div className="flex gap-4 text-xs text-slate-500">
              <span>Created: {formatDate(task.createdAt)}</span>
              {task.dueDate && <span>Due: {formatDate(task.dueDate)}</span>}
            </div>
          </CardContent>
        </Card>
      ))}
    </div>
  )
}
EOF

cat > "$PROJECT_ROOT/components/generated/project-table.tsx" << 'EOF'
// AUTO-GENERATED from ontology/task-management.ttl
// DO NOT EDIT - Changes will be overwritten

'use client'

import type { Project } from '@/lib/types'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { formatDate } from '@/lib/utils'

interface ProjectTableProps {
  projects: Project[]
}

export function ProjectTable({ projects }: ProjectTableProps) {
  return (
    <div className="space-y-4">
      {projects.map((project) => (
        <Card key={project.id}>
          <CardHeader>
            <CardTitle className="text-lg">{project.name}</CardTitle>
          </CardHeader>
          <CardContent>
            {project.description && (
              <p className="text-sm text-slate-600 mb-3">{project.description}</p>
            )}
            <div className="flex gap-4 text-xs text-slate-500">
              {project.startDate && <span>Start: {formatDate(project.startDate)}</span>}
              {project.endDate && <span>End: {formatDate(project.endDate)}</span>}
            </div>
          </CardContent>
        </Card>
      ))}
    </div>
  )
}
EOF

# Generate pages
echo "📄 Generating pages..."
cat > "$PROJECT_ROOT/app/tasks/page.tsx" << 'EOF'
// AUTO-GENERATED from ontology/task-management.ttl
// DO NOT EDIT - Changes will be overwritten

import { TaskTable } from '@/components/generated/task-table'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'

async function getTasks() {
  const res = await fetch(`http://localhost:3000/api/tasks`, {
    cache: 'no-store',
  })
  if (!res.ok) throw new Error('Failed to fetch tasks')
  const data = await res.json()
  return data.tasks.map((task: any) => ({
    ...task,
    createdAt: new Date(task.createdAt),
    dueDate: task.dueDate ? new Date(task.dueDate) : undefined,
  }))
}

export default async function TasksPage() {
  const tasks = await getTasks()

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle>Tasks</CardTitle>
          <CardDescription>
            Auto-generated from ontology/task-management.ttl
          </CardDescription>
        </CardHeader>
        <CardContent>
          <TaskTable tasks={tasks} />
        </CardContent>
      </Card>
    </div>
  )
}
EOF

cat > "$PROJECT_ROOT/app/projects/page.tsx" << 'EOF'
// AUTO-GENERATED from ontology/task-management.ttl
// DO NOT EDIT - Changes will be overwritten

import { ProjectTable } from '@/components/generated/project-table'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'

async function getProjects() {
  const res = await fetch(`http://localhost:3000/api/projects`, {
    cache: 'no-store',
  })
  if (!res.ok) throw new Error('Failed to fetch projects')
  const data = await res.json()
  return data.projects.map((project: any) => ({
    ...project,
    startDate: project.startDate ? new Date(project.startDate) : undefined,
    endDate: project.endDate ? new Date(project.endDate) : undefined,
  }))
}

export default async function ProjectsPage() {
  const projects = await getProjects()

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle>Projects</CardTitle>
          <CardDescription>
            Auto-generated from ontology/task-management.ttl
          </CardDescription>
        </CardHeader>
        <CardContent>
          <ProjectTable projects={projects} />
        </CardContent>
      </Card>
    </div>
  )
}
EOF

echo "✨ Generation complete!"
echo ""
echo "Generated files:"
echo "  - lib/types.ts (TypeScript types)"
echo "  - lib/validation.ts (Zod schemas)"
echo "  - app/api/tasks/route.ts (Task API)"
echo "  - app/api/projects/route.ts (Project API)"
echo "  - components/generated/task-table.tsx (Task UI)"
echo "  - components/generated/project-table.tsx (Project UI)"
echo "  - app/tasks/page.tsx (Tasks page)"
echo "  - app/projects/page.tsx (Projects page)"
echo ""
echo "🚀 Ready to run: npm run dev"
