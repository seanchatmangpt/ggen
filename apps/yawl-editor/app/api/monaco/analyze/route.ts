import { analyzeCode } from '@/lib/ai-code-service'
import type { CodeAnalysisRequest } from '@/lib/ai-code-service'

export async function POST(request: Request) {
  try {
    const body = (await request.json()) as CodeAnalysisRequest

    // Validate request
    if (!body.code || !body.language) {
      return new Response('Missing required fields', { status: 400 })
    }

    // Analyze code
    const suggestions = await analyzeCode({
      code: body.code,
      language: body.language,
      type: body.type || 'refactor',
    })

    return new Response(JSON.stringify(suggestions), {
      headers: { 'Content-Type': 'application/json' },
    })
  } catch (error) {
    console.error('Code analysis error:', error)
    return new Response('Failed to analyze code', { status: 500 })
  }
}
