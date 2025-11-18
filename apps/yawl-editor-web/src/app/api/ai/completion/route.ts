export const runtime = "nodejs"

interface CompletionRequest {
  prompt: string
  context?: string
  maxTokens?: number
}

export async function POST(request: Request) {
  try {
    const { prompt } = (await request.json()) as CompletionRequest

    if (!prompt) {
      return new Response(JSON.stringify({ error: "Prompt is required" }), {
        status: 400,
        headers: { "Content-Type": "application/json" },
      })
    }

    // Mock AI response for demonstration
    // In production, replace with actual OpenAI API call
    const mockCompletions: Record<string, string> = {
      workflow:
        "Here's a suggested YAWL workflow:\n\n```json\n{\n  \"name\": \"Sample Workflow\",\n  \"tasks\": [],\n  \"flows\": []\n}\n```",
      analyze:
        "Analysis complete:\n\n✓ Workflow structure is valid\n⚠️ Consider adding error handling\n⚠️ Define input/output specifications",
      optimize:
        "Optimization suggestions:\n\n• Reduce task complexity\n• Add parallel task execution where possible\n• Implement proper error recovery",
    }

    const isWorkflow = prompt.toLowerCase().includes("workflow")
    const isOptimize = prompt.toLowerCase().includes("suggest optimizations") || prompt.toLowerCase().includes("optimize")

    let completion = mockCompletions.analyze
    if (isWorkflow) completion = mockCompletions.workflow
    else if (isOptimize) completion = mockCompletions.optimize

    // Stream the response
    const encoder = new TextEncoder()
    const customReadable = new ReadableStream({
      start(controller) {
        controller.enqueue(encoder.encode(completion))
        controller.close()
      },
    })

    return new Response(customReadable, {
      headers: {
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        Connection: "keep-alive",
      },
    })
  } catch (error) {
    console.error("AI completion error:", error)
    return new Response(
      JSON.stringify({
        error: error instanceof Error ? error.message : "Failed to generate completion",
      }),
      {
        status: 500,
        headers: { "Content-Type": "application/json" },
      }
    )
  }
}
