import { NextRequest, NextResponse } from 'next/server'
import { generateCodeCompletion } from '@/lib/ai/suggestions'

export async function POST(request: NextRequest) {
  try {
    const { code, language, type } = await request.json()

    if (!code || !language) {
      return NextResponse.json(
        { error: 'Missing code or language' },
        { status: 400 }
      )
    }

    const suggestions = await generateCodeCompletion(code, language, type)

    return NextResponse.json({
      suggestions,
      timestamp: new Date().toISOString(),
    })
  } catch (error) {
    console.error('Suggestion generation error:', error)
    return NextResponse.json(
      { error: 'Failed to generate suggestions' },
      { status: 500 }
    )
  }
}
