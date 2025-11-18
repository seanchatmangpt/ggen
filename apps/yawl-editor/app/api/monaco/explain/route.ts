import { explainCode } from '@/lib/ai-code-service'

export async function POST(request: Request) {
  try {
    const body = await request.json()

    if (!body.code || !body.language) {
      return new Response('Missing required fields', { status: 400 })
    }

    // Stream explanation
    const encoder = new TextEncoder()
    const explainStream = async function* () {
      const explanation = await explainCode(body.code, body.language)
      yield explanation
    }

    const stream = new ReadableStream({
      async start(controller) {
        try {
          for await (const chunk of explainStream()) {
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
    console.error('Explanation error:', error)
    return new Response('Failed to explain code', { status: 500 })
  }
}
