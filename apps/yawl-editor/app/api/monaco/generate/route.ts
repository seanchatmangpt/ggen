import { generateCodeStream } from '@/lib/ai-code-service'
import type { CodeGenerationRequest } from '@/lib/ai-code-service'

export async function POST(request: Request) {
  try {
    const body = (await request.json()) as CodeGenerationRequest

    // Validate request
    if (!body.language || !body.description) {
      return new Response('Missing required fields', { status: 400 })
    }

    // Stream code generation
    const encoder = new TextEncoder()
    const stream = new ReadableStream({
      async start(controller) {
        try {
          for await (const chunk of generateCodeStream(body)) {
            controller.enqueue(encoder.encode(chunk))
          }
          controller.close()
        } catch (error) {
          controller.error(error)
        }
      },
    })

    return new Response(stream, {
      headers: {
        'Content-Type': 'text/event-stream',
        'Cache-Control': 'no-cache',
      },
    })
  } catch (error) {
    console.error('Code generation error:', error)
    return new Response('Failed to generate code', { status: 500 })
  }
}
