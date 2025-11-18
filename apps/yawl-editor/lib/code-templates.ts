/**
 * Pre-built Code Templates for AI Generation
 * Starter templates for services, components, hooks, etc.
 */

export interface CodeTemplate {
  id: string
  name: string
  description: string
  language: string
  category: 'service' | 'component' | 'hook' | 'utility' | 'test'
  code: string
  tags: string[]
}

export const codeTemplates: CodeTemplate[] = [
  {
    id: 'ts-service',
    name: 'TypeScript Service',
    description: 'Basic TypeScript service with error handling',
    language: 'typescript',
    category: 'service',
    tags: ['typescript', 'service', 'api'],
    code: `/**
 * Service description
 */
export class MyService {
  constructor(private baseUrl: string) {}

  /**
   * Fetch data from API
   */
  async fetchData(id: string): Promise<any> {
    try {
      const response = await fetch(\`\${this.baseUrl}/\${id}\`)
      if (!response.ok) {
        throw new Error(\`HTTP error! status: \${response.status}\`)
      }
      return await response.json()
    } catch (error) {
      console.error('Failed to fetch data:', error)
      throw error
    }
  }

  /**
   * Create new resource
   */
  async create(data: any): Promise<any> {
    try {
      const response = await fetch(this.baseUrl, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      })
      if (!response.ok) {
        throw new Error(\`HTTP error! status: \${response.status}\`)
      }
      return await response.json()
    } catch (error) {
      console.error('Failed to create resource:', error)
      throw error
    }
  }
}`,
  },
  {
    id: 'react-component',
    name: 'React Component',
    description: 'Functional React component with hooks',
    language: 'typescript',
    category: 'component',
    tags: ['react', 'component', 'typescript'],
    code: `'use client'

import { useState, useCallback } from 'react'

interface MyComponentProps {
  title: string
  onSubmit?: (data: any) => void
}

/**
 * My Component
 * Component description here
 */
export function MyComponent({ title, onSubmit }: MyComponentProps) {
  const [isLoading, setIsLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const handleSubmit = useCallback(async (formData: any) => {
    setIsLoading(true)
    setError(null)

    try {
      // Handle submission
      onSubmit?.(formData)
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Unknown error')
    } finally {
      setIsLoading(false)
    }
  }, [onSubmit])

  return (
    <div>
      <h2>{title}</h2>
      {error && <div className="error">{error}</div>}
      {isLoading && <div>Loading...</div>}
      {/* Component content */}
    </div>
  )
}`,
  },
  {
    id: 'react-hook',
    name: 'React Hook',
    description: 'Custom React hook template',
    language: 'typescript',
    category: 'hook',
    tags: ['react', 'hook', 'typescript'],
    code: `import { useState, useCallback, useEffect } from 'react'

interface UseMyHookOptions {
  onSuccess?: (data: any) => void
  onError?: (error: Error) => void
}

/**
 * Custom hook for specific functionality
 */
export function useMyHook(options: UseMyHookOptions = {}) {
  const [data, setData] = useState<any>(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<Error | null>(null)

  const execute = useCallback(async () => {
    setLoading(true)
    setError(null)

    try {
      // Perform operation
      setData(null)
      options.onSuccess?.(data)
    } catch (err) {
      const error = err instanceof Error ? err : new Error('Unknown error')
      setError(error)
      options.onError?.(error)
    } finally {
      setLoading(false)
    }
  }, [options])

  useEffect(() => {
    // Cleanup if needed
    return () => {}
  }, [])

  return { data, loading, error, execute }
}`,
  },
  {
    id: 'jest-test',
    name: 'Jest Test Suite',
    description: 'Jest unit test template',
    language: 'typescript',
    category: 'test',
    tags: ['jest', 'testing', 'typescript'],
    code: `import { describe, it, expect, beforeEach, afterEach } from '@jest/globals'
import { MyService } from './my-service'

describe('MyService', () => {
  let service: MyService

  beforeEach(() => {
    service = new MyService('http://api.example.com')
  })

  afterEach(() => {
    // Cleanup
  })

  describe('fetchData', () => {
    it('should fetch data successfully', async () => {
      const data = await service.fetchData('123')
      expect(data).toBeDefined()
    })

    it('should handle errors', async () => {
      await expect(service.fetchData('invalid')).rejects.toThrow()
    })
  })

  describe('create', () => {
    it('should create new resource', async () => {
      const created = await service.create({ name: 'Test' })
      expect(created).toHaveProperty('id')
    })
  })
})`,
  },
  {
    id: 'next-api-route',
    name: 'Next.js API Route',
    description: 'Next.js API route handler',
    language: 'typescript',
    category: 'service',
    tags: ['nextjs', 'api', 'typescript'],
    code: `import type { NextRequest } from 'next/server'
import { NextResponse } from 'next/server'

/**
 * GET handler
 */
export async function GET(request: NextRequest) {
  try {
    const searchParams = request.nextUrl.searchParams
    const id = searchParams.get('id')

    // Process request
    const data = { id, message: 'Success' }

    return NextResponse.json(data, { status: 200 })
  } catch (error) {
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    )
  }
}

/**
 * POST handler
 */
export async function POST(request: NextRequest) {
  try {
    const body = await request.json()

    // Process request
    const result = { success: true, data: body }

    return NextResponse.json(result, { status: 201 })
  } catch (error) {
    return NextResponse.json(
      { error: 'Failed to process request' },
      { status: 400 }
    )
  }
}`,
  },
  {
    id: 'zod-schema',
    name: 'Zod Validation Schema',
    description: 'Zod schema for data validation',
    language: 'typescript',
    category: 'utility',
    tags: ['zod', 'validation', 'typescript'],
    code: `import { z } from 'zod'

/**
 * User schema
 */
export const UserSchema = z.object({
  id: z.string().min(1),
  email: z.string().email(),
  name: z.string().min(1),
  age: z.number().int().positive().optional(),
  roles: z.array(z.string()),
  createdAt: z.date(),
})

export type User = z.infer<typeof UserSchema>

/**
 * Create user schema (without id and timestamps)
 */
export const CreateUserSchema = UserSchema.omit({
  id: true,
  createdAt: true,
})

export type CreateUserInput = z.infer<typeof CreateUserSchema>`,
  },
]

/**
 * Get template by ID
 */
export function getTemplate(id: string): CodeTemplate | undefined {
  return codeTemplates.find((t) => t.id === id)
}

/**
 * Get templates by category
 */
export function getTemplatesByCategory(
  category: CodeTemplate['category']
): CodeTemplate[] {
  return codeTemplates.filter((t) => t.category === category)
}

/**
 * Get templates by language
 */
export function getTemplatesByLanguage(language: string): CodeTemplate[] {
  return codeTemplates.filter((t) => t.language === language)
}

/**
 * Search templates
 */
export function searchTemplates(query: string): CodeTemplate[] {
  const lower = query.toLowerCase()
  return codeTemplates.filter(
    (t) =>
      t.name.toLowerCase().includes(lower) ||
      t.description.toLowerCase().includes(lower) ||
      t.tags.some((tag) => tag.toLowerCase().includes(lower))
  )
}
