import { NextRequest, NextResponse } from 'next/server'
import { createDefaultSparqlClient } from '@/lib/sparql-client'
import { SPARQL_QUERIES } from '@/lib/sparql-queries'
import { CreateCaseSchema, validateCreateCase } from '@/lib/validation'

/**
 * GET /api/cases
 * List all cases with optional filtering
 */
export async function GET(request: NextRequest) {
  try {
    const searchParams = request.nextUrl.searchParams
    const status = searchParams.get('status')

    const client = createDefaultSparqlClient()

    let query = SPARQL_QUERIES.getAllCases
    if (status) {
      query = SPARQL_QUERIES.getCasesByStatus(status)
    }

    const result = await client.query(query)

    // Normalize results
    const cases = result.bindings.map((binding: any) => {
      const item: Record<string, any> = {}
      for (const [key, value] of Object.entries(binding)) {
        if (typeof value === 'object' && value !== null && 'value' in value) {
          item[key] = (value as any).value
        } else {
          item[key] = value
        }
      }
      return item
    })

    return NextResponse.json({
      success: true,
      data: cases,
      total: cases.length,
    })
  } catch (error) {
    console.error('Error fetching cases:', error)
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    )
  }
}

/**
 * POST /api/cases
 * Create a new case
 */
export async function POST(request: NextRequest) {
  try {
    const body = await request.json()

    // Validate input
    const validation = validateCreateCase(body)
    if (!validation.success) {
      return NextResponse.json(
        {
          success: false,
          error: validation.error,
        },
        { status: 400 }
      )
    }

    const { processId, ownerId } = validation.data
    const caseId = `case-${Date.now()}`

    const client = createDefaultSparqlClient()

    // Create the case via SPARQL
    const updateQuery = SPARQL_QUERIES.createCase(caseId, processId, ownerId || '')
    await client.update(updateQuery)

    // Return the created case
    return NextResponse.json(
      {
        success: true,
        data: {
          id: caseId,
          processId,
          ownerId: ownerId || null,
        },
        message: 'Case created successfully',
      },
      { status: 201 }
    )
  } catch (error) {
    console.error('Error creating case:', error)
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    )
  }
}
