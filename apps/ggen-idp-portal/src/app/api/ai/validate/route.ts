import { NextRequest, NextResponse } from 'next/server'
import { validateCode } from '@/lib/ai/validation'

export async function POST(request: NextRequest) {
  try {
    const { code, language } = await request.json()

    if (!code || !language) {
      return NextResponse.json(
        { error: 'Missing code or language' },
        { status: 400 }
      )
    }

    const validationResult = await validateCode(code, language)

    return NextResponse.json({
      ...validationResult,
      timestamp: new Date().toISOString(),
    })
  } catch (error) {
    console.error('Validation error:', error)
    return NextResponse.json(
      { error: 'Validation failed' },
      { status: 500 }
    )
  }
}
