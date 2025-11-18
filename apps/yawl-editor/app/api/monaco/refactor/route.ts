import { refactorCode } from '@/lib/ai-code-service'

export async function POST(request: Request) {
  try {
    const body = await request.json()

    if (!body.code || !body.language) {
      return new Response('Missing required fields', { status: 400 })
    }

    // Stream refactored code
    const encoder = new TextEncoder()
    const refactorStream = async function* () {
      const refactored = await refactorCode(body.code, body.language, body.focusArea)
      yield refactored
    }

    const stream = new ReadableStream({
      async start(controller) {
        try {
          for await (const chunk of refactorStream()) {
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
    console.error('Refactoring error:', error)
    return new Response('Failed to refactor code', { status: 500 })
  }
}
