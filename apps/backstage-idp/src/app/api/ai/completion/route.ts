export const runtime = "nodejs"

interface CompletionRequest {
  prompt: string
  context?: string
  maxTokens?: number
  type?: "service" | "template" | "documentation" | "general"
}

export async function POST(request: Request) {
  try {
    const { prompt, context = "", maxTokens = 500, type = "general" } =
      (await request.json()) as CompletionRequest

    if (!prompt) {
      return new Response(JSON.stringify({ error: "Prompt is required" }), {
        status: 400,
        headers: { "Content-Type": "application/json" },
      })
    }

    // Mock AI responses for demonstration
    // In production, replace with actual OpenAI API call
    const mockResponses: Record<string, string> = {
      service: `# Service Configuration\n\n## Overview\nThis is a well-designed microservice that follows best practices.\n\n## Key Components\n- API Endpoints\n- Database Layer\n- Authentication\n- Error Handling\n\n## Recommendations\n- Add rate limiting\n- Implement caching strategy\n- Set up proper monitoring`,
      template: `# Project Template\n\n## Directory Structure\n\`\`\`\nproject/\n├── src/\n├── tests/\n├── docs/\n├── package.json\n└── README.md\n\`\`\`\n\n## Dependencies\n- typescript\n- jest\n- eslint\n\n## Getting Started\n\`npm install && npm start\``,
      documentation: `# Service Documentation\n\n## Overview\nClear description of the service purpose and functionality.\n\n## Usage\nStep-by-step guide with examples.\n\n## API Reference\nDetailed endpoint documentation.\n\n## Troubleshooting\nCommon issues and solutions.`,
      general: `# IDP Analysis\n\n✓ Service catalog well organized\n✓ Templates provide good coverage\n⚠️ Consider team growth strategies\n⚠️ Documentation could be expanded\n\n## Recommendations\n1. Add more service templates\n2. Improve cross-team collaboration\n3. Implement service discovery`,
    }

    const completion = mockResponses[type] || mockResponses.general

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
