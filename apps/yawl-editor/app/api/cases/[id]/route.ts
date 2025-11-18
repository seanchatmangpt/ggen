import { NextRequest, NextResponse } from 'next/server'
import { createDefaultSparqlClient } from '@/lib/sparql-client'
import { SPARQL_QUERIES } from '@/lib/sparql-queries'
import { UpdateCaseSchema, validateCase } from '@/lib/validation'

/**
 * GET /api/cases/:id
 * Get a specific case
 */
export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const { id } = params
    const client = createDefaultSparqlClient()

    const query = SPARQL_QUERIES.getCaseById(`http://yawlfoundation.org/data#${id}`)
    const result = await client.query(query)

    if (result.bindings.length === 0) {
      return NextResponse.json(
        { success: false, error: 'Case not found' },
        { status: 404 }
      )
    }

    const caseData = result.bindings[0]
    const normalized: Record<string, any> = {}

    for (const [key, value] of Object.entries(caseData)) {
      if (typeof value === 'object' && value !== null && 'value' in value) {
        normalized[key] = (value as any).value
      } else {
        normalized[key] = value
      }
    }

    return NextResponse.json({
      success: true,
      data: normalized,
    })
  } catch (error) {
    console.error('Error fetching case:', error)
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
 * PUT /api/cases/:id
 * Update a case
 */
export async function PUT(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const { id } = params
    const body = await request.json()

    // Validate update data
    const validation = UpdateCaseSchema.safeParse(body)
    if (!validation.success) {
      return NextResponse.json(
        {
          success: false,
          error: validation.error.message,
        },
        { status: 400 }
      )
    }

    const { status } = validation.data
    const client = createDefaultSparqlClient()

    // Update the case via SPARQL
    if (status) {
      const updateQuery = SPARQL_QUERIES.updateCaseStatus(
        `http://yawlfoundation.org/data#${id}`,
        status
      )
      await client.update(updateQuery)
    }

    return NextResponse.json({
      success: true,
      message: 'Case updated successfully',
    })
  } catch (error) {
    console.error('Error updating case:', error)
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
 * DELETE /api/cases/:id
 * Delete a case
 */
export async function DELETE(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const { id } = params
    const client = createDefaultSparqlClient()

    // Delete pattern
    const deletePattern = `<http://yawlfoundation.org/data#${id}> ?p ?o`
    await client.deleteData(deletePattern)

    return NextResponse.json({
      success: true,
      message: 'Case deleted successfully',
    })
  } catch (error) {
    console.error('Error deleting case:', error)
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    )
  }
}
