/**
 * Agent Swarm REST API
 * POST /api/agents/swarm - Create and manage agent swarms
 */

import { NextRequest, NextResponse } from 'next/server'
import { z } from 'zod'
import { getSwarmAPI } from '@/lib/agents/swarm-api'

// Request validation schemas
const CreateSwarmSchema = z.object({
  swarmId: z.string().min(1),
  agents: z
    .array(
      z.object({
        type: z.enum(['coordinator', 'worker', 'scout', 'learner']),
        name: z.string(),
        options: z.record(z.any()).optional(),
      })
    )
    .optional(),
  config: z.record(z.any()).optional(),
})

const QueueTaskSchema = z.object({
  type: z.string(),
  priority: z.enum(['low', 'normal', 'high', 'critical']),
  goal: z.string(),
})

const ConsensusSchema = z.object({
  proposal: z.record(z.any()),
})

/**
 * GET /api/agents/swarm
 * List swarms or get specific swarm info
 */
export async function GET(request: NextRequest) {
  try {
    const api = getSwarmAPI()
    const swarmId = request.nextUrl.searchParams.get('swarmId')
    const action = request.nextUrl.searchParams.get('action')

    if (!swarmId) {
      // List all swarms
      const swarms = api.listSwarms()
      return NextResponse.json({
        success: true,
        swarms,
        count: swarms.length,
      })
    }

    const swarm = api.getSwarm(swarmId)
    if (!swarm) {
      return NextResponse.json({ success: false, error: 'Swarm not found' }, { status: 404 })
    }

    // Get different types of information
    switch (action) {
      case 'metrics':
        return NextResponse.json({
          success: true,
          metrics: api.getSwarmMetrics(swarmId),
        })

      case 'report':
        return NextResponse.json({
          success: true,
          report: api.getSwarmReport(swarmId),
        })

      case 'health':
        return NextResponse.json({
          success: true,
          health: api.getHealthReport(swarmId),
        })

      case 'agents':
        return NextResponse.json({
          success: true,
          agents: api
            .listAgents(swarmId)
            .map((a) => ({
              id: a.getId(),
              name: a.getName(),
              role: a.getRole(),
              status: a.getStatus(),
              metrics: a.getMetrics(),
            })),
        })

      case 'tasks':
        return NextResponse.json({
          success: true,
          queue: api.getTaskQueueStatus(swarmId),
        })

      default:
        return NextResponse.json({
          success: true,
          swarmId,
          status: api.getStatus(),
          metrics: api.getSwarmMetrics(swarmId),
        })
    }
  } catch (error) {
    console.error('GET /api/agents/swarm error:', error)
    return NextResponse.json(
      { success: false, error: 'Internal server error' },
      { status: 500 }
    )
  }
}

/**
 * POST /api/agents/swarm
 * Create swarm or perform swarm operations
 */
export async function POST(request: NextRequest) {
  try {
    const api = getSwarmAPI()
    const body = await request.json()

    // Determine operation
    const operation = request.nextUrl.searchParams.get('action') || 'create'

    switch (operation) {
      case 'create': {
        const validated = CreateSwarmSchema.parse(body)

        const swarm = validated.agents
          ? api.createSwarmWithAgents(validated.swarmId, validated.agents, validated.config)
          : api.createSwarm(validated.swarmId, validated.config)

        if (!swarm) {
          return NextResponse.json(
            { success: false, error: 'Failed to create swarm' },
            { status: 400 }
          )
        }

        return NextResponse.json({
          success: true,
          message: 'Swarm created successfully',
          swarmId: validated.swarmId,
          agentCount: api.listAgents(validated.swarmId).length,
        })
      }

      case 'queue-task': {
        const validated = QueueTaskSchema.parse(body)
        const swarmId = request.nextUrl.searchParams.get('swarmId')

        if (!swarmId) {
          return NextResponse.json(
            { success: false, error: 'swarmId required' },
            { status: 400 }
          )
        }

        const success = api.queueTask(swarmId, validated as any)

        return NextResponse.json({
          success,
          message: success ? 'Task queued' : 'Failed to queue task',
          queue: api.getTaskQueueStatus(swarmId),
        })
      }

      case 'consensus': {
        const validated = ConsensusSchema.parse(body)
        const swarmId = request.nextUrl.searchParams.get('swarmId')

        if (!swarmId) {
          return NextResponse.json(
            { success: false, error: 'swarmId required' },
            { status: 400 }
          )
        }

        const result = await api.requestConsensus(swarmId, validated.proposal)

        return NextResponse.json({
          success: true,
          consensus: result,
        })
      }

      case 'optimize-pso': {
        const swarmId = request.nextUrl.searchParams.get('swarmId')
        if (!swarmId) {
          return NextResponse.json(
            { success: false, error: 'swarmId required' },
            { status: 400 }
          )
        }

        const agents = api.listAgents(swarmId)
        const result = api.optimizeWithPSO(
          swarmId,
          agents.map((a) => a.getId()),
          body.dimensions || 5,
          body.iterations || 100
        )

        return NextResponse.json({
          success: true,
          algorithm: 'pso',
          result,
        })
      }

      case 'optimize-aco': {
        const swarmId = request.nextUrl.searchParams.get('swarmId')
        if (!swarmId) {
          return NextResponse.json(
            { success: false, error: 'swarmId required' },
            { status: 400 }
          )
        }

        const agents = api.listAgents(swarmId)
        const result = api.optimizeWithACO(
          swarmId,
          agents.map((a) => a.getId()),
          body.iterations || 50,
          body.antCount || 10
        )

        return NextResponse.json({
          success: true,
          algorithm: 'aco',
          result,
        })
      }

      case 'learn': {
        const swarmId = request.nextUrl.searchParams.get('swarmId')
        if (!swarmId) {
          return NextResponse.json(
            { success: false, error: 'swarmId required' },
            { status: 400 }
          )
        }

        api.triggerLearning(swarmId)

        return NextResponse.json({
          success: true,
          message: 'Swarm learning triggered',
        })
      }

      case 'heal': {
        const swarmId = request.nextUrl.searchParams.get('swarmId')
        if (!swarmId) {
          return NextResponse.json(
            { success: false, error: 'swarmId required' },
            { status: 400 }
          )
        }

        await api.monitorAndHeal(swarmId)

        return NextResponse.json({
          success: true,
          message: 'Swarm health monitoring and healing executed',
          health: api.getHealthReport(swarmId),
        })
      }

      default:
        return NextResponse.json(
          { success: false, error: 'Unknown operation' },
          { status: 400 }
        )
    }
  } catch (error) {
    console.error('POST /api/agents/swarm error:', error)

    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { success: false, error: 'Invalid request format', details: error.errors },
        { status: 400 }
      )
    }

    return NextResponse.json(
      { success: false, error: 'Internal server error' },
      { status: 500 }
    )
  }
}

/**
 * DELETE /api/agents/swarm
 * Delete a swarm
 */
export async function DELETE(request: NextRequest) {
  try {
    const api = getSwarmAPI()
    const swarmId = request.nextUrl.searchParams.get('swarmId')

    if (!swarmId) {
      return NextResponse.json(
        { success: false, error: 'swarmId required' },
        { status: 400 }
      )
    }

    const success = api.deleteSwarm(swarmId)

    return NextResponse.json({
      success,
      message: success ? 'Swarm deleted' : 'Swarm not found',
    })
  } catch (error) {
    console.error('DELETE /api/agents/swarm error:', error)
    return NextResponse.json(
      { success: false, error: 'Internal server error' },
      { status: 500 }
    )
  }
}
