/**
 * Vercel AI Service for Code Generation and Analysis
 * Powered by Claude AI with streaming support
 */

import { Anthropic } from '@anthropic-ai/sdk'

const client = new Anthropic()

export interface CodeGenerationRequest {
  language: string
  type: 'service' | 'component' | 'hook' | 'utility' | 'template'
  description: string
  context?: string
  requirements?: string[]
}

export interface CodeAnalysisRequest {
  code: string
  language: string
  type: 'refactor' | 'optimize' | 'security' | 'performance' | 'documentation'
}

export interface CodeSuggestion {
  type: 'fix' | 'suggestion' | 'refactor' | 'optimize'
  line: number
  message: string
  code?: string
  explanation: string
}

export interface CompletionSuggestion {
  label: string
  kind: 'snippet' | 'class' | 'function' | 'variable' | 'keyword'
  insertText: string
  detail: string
  documentation: string
}

/**
 * Generate code using Claude AI
 */
export async function generateCode(request: CodeGenerationRequest): Promise<string> {
  const prompt = `You are an expert ${request.language} developer. Generate production-ready code for the following:

Type: ${request.type}
Description: ${request.description}
${request.requirements ? `Requirements:\n${request.requirements.map(r => `- ${r}`).join('\n')}` : ''}
${request.context ? `Context:\n${request.context}` : ''}

Generate clean, well-documented, type-safe code with:
- Proper error handling
- Comprehensive JSDoc comments
- Following modern best practices
- Production-ready quality

Return only the code without explanations.`

  const stream = client.messages.stream({
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 4096,
    messages: [{ role: 'user', content: prompt }],
  })

  let fullResponse = ''
  for await (const chunk of stream) {
    if (chunk.type === 'content_block_delta' && chunk.delta.type === 'text_delta') {
      fullResponse += chunk.delta.text
    }
  }

  return fullResponse
}

/**
 * Stream code generation with real-time updates
 */
export async function* generateCodeStream(
  request: CodeGenerationRequest
): AsyncGenerator<string> {
  const prompt = `You are an expert ${request.language} developer. Generate production-ready code for:

Type: ${request.type}
Description: ${request.description}
${request.requirements ? `Requirements:\n${request.requirements.map(r => `- ${r}`).join('\n')}` : ''}

Generate clean, well-documented code following best practices.
Return only the code without explanations.`

  const stream = client.messages.stream({
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 4096,
    messages: [{ role: 'user', content: prompt }],
  })

  for await (const chunk of stream) {
    if (chunk.type === 'content_block_delta' && chunk.delta.type === 'text_delta') {
      yield chunk.delta.text
    }
  }
}

/**
 * Analyze code for issues and improvements
 */
export async function analyzeCode(request: CodeAnalysisRequest): Promise<CodeSuggestion[]> {
  const analysisTypes = {
    refactor: 'refactoring opportunities and code quality improvements',
    optimize: 'performance optimizations',
    security: 'security vulnerabilities and best practices',
    performance: 'performance bottlenecks',
    documentation: 'missing documentation and comments',
  }

  const prompt = `Analyze this ${request.language} code for ${analysisTypes[request.type]}:

\`\`\`${request.language}
${request.code}
\`\`\`

Provide suggestions as JSON array with this format:
[
  {
    "type": "${request.type}",
    "line": <line number>,
    "message": "<short message>",
    "code": "<suggested code snippet if applicable>",
    "explanation": "<detailed explanation>"
  }
]

Return only valid JSON, no other text.`

  const response = await client.messages.create({
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 2048,
    messages: [{ role: 'user', content: prompt }],
  })

  try {
    const content =
      response.content[0].type === 'text' ? response.content[0].text : ''
    const jsonMatch = content.match(/\[[\s\S]*\]/)
    if (jsonMatch) {
      return JSON.parse(jsonMatch[0])
    }
  } catch (err) {
    console.error('Failed to parse analysis response:', err)
  }

  return []
}

/**
 * Get code completions using AI
 */
export async function getCodeCompletions(
  code: string,
  language: string,
  position: number
): Promise<CompletionSuggestion[]> {
  const beforeCursor = code.substring(0, position)
  const afterCursor = code.substring(position)

  const prompt = `You are an expert ${language} code completion engine.

Current code:
\`\`\`${language}
${beforeCursor}█${afterCursor}
\`\`\`

Provide 5 smart completion suggestions as JSON array:
[
  {
    "label": "<completion text>",
    "kind": "function|class|variable|keyword|snippet",
    "insertText": "<full text to insert>",
    "detail": "<type info>",
    "documentation": "<markdown documentation>"
  }
]

Focus on contextual, useful completions. Return only JSON.`

  try {
    const response = await client.messages.create({
      model: 'claude-3-5-sonnet-20241022',
      max_tokens: 1024,
      messages: [{ role: 'user', content: prompt }],
    })

    const content =
      response.content[0].type === 'text' ? response.content[0].text : ''
    const jsonMatch = content.match(/\[[\s\S]*\]/)
    if (jsonMatch) {
      return JSON.parse(jsonMatch[0])
    }
  } catch (err) {
    console.error('Failed to get completions:', err)
  }

  return []
}

/**
 * Generate documentation from code
 */
export async function generateDocumentation(
  code: string,
  language: string
): Promise<string> {
  const prompt = `Generate comprehensive markdown documentation for this ${language} code:

\`\`\`${language}
${code}
\`\`\`

Include:
- Function/class descriptions
- Parameters and return types
- Usage examples
- Error handling notes
- Best practices

Return only markdown documentation.`

  const stream = client.messages.stream({
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 2048,
    messages: [{ role: 'user', content: prompt }],
  })

  let fullResponse = ''
  for await (const chunk of stream) {
    if (chunk.type === 'content_block_delta' && chunk.delta.type === 'text_delta') {
      fullResponse += chunk.delta.text
    }
  }

  return fullResponse
}

/**
 * Get code refactoring suggestions
 */
export async function refactorCode(
  code: string,
  language: string,
  focusArea?: string
): Promise<string> {
  const prompt = `Refactor this ${language} code${focusArea ? ` focusing on ${focusArea}` : ''}:

\`\`\`${language}
${code}
\`\`\`

Provide improved version that:
- Follows modern ${language} best practices
- Improves readability and maintainability
- Optimizes performance where possible
- Maintains same functionality

Return only the refactored code without explanations.`

  const stream = client.messages.stream({
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 4096,
    messages: [{ role: 'user', content: prompt }],
  })

  let fullResponse = ''
  for await (const chunk of stream) {
    if (chunk.type === 'content_block_delta' && chunk.delta.type === 'text_delta') {
      fullResponse += chunk.delta.text
    }
  }

  return fullResponse
}

/**
 * Explain code functionality
 */
export async function explainCode(
  code: string,
  language: string
): Promise<string> {
  const prompt = `Explain this ${language} code in detail:

\`\`\`${language}
${code}
\`\`\`

Provide:
- What the code does
- How it works
- Key algorithms or patterns used
- Potential issues or limitations

Use markdown formatting with code blocks.`

  const stream = client.messages.stream({
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 2048,
    messages: [{ role: 'user', content: prompt }],
  })

  let fullResponse = ''
  for await (const chunk of stream) {
    if (chunk.type === 'content_block_delta' && chunk.delta.type === 'text_delta') {
      fullResponse += chunk.delta.text
    }
  }

  return fullResponse
}

/**
 * Convert code between languages
 */
export async function convertCode(
  code: string,
  fromLanguage: string,
  toLanguage: string
): Promise<string> {
  const prompt = `Convert this ${fromLanguage} code to ${toLanguage}:

\`\`\`${fromLanguage}
${code}
\`\`\`

Maintain the same functionality and follow ${toLanguage} best practices.
Return only the converted code without explanations.`

  const stream = client.messages.stream({
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 4096,
    messages: [{ role: 'user', content: prompt }],
  })

  let fullResponse = ''
  for await (const chunk of stream) {
    if (chunk.type === 'content_block_delta' && chunk.delta.type === 'text_delta') {
      fullResponse += chunk.delta.text
    }
  }

  return fullResponse
}

/**
 * Generate test code from implementation
 */
export async function generateTests(
  code: string,
  language: string,
  testFramework: string = 'jest'
): Promise<string> {
  const prompt = `Generate comprehensive ${testFramework} tests for this ${language} code:

\`\`\`${language}
${code}
\`\`\`

Create:
- Unit tests for each function
- Edge case tests
- Error handling tests
- Integration test examples

Return only the test code without explanations.`

  const stream = client.messages.stream({
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 4096,
    messages: [{ role: 'user', content: prompt }],
  })

  let fullResponse = ''
  for await (const chunk of stream) {
    if (chunk.type === 'content_block_delta' && chunk.delta.type === 'text_delta') {
      fullResponse += chunk.delta.text
    }
  }

  return fullResponse
}
