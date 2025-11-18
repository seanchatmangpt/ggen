/**
 * API Validation Tests
 * Comprehensive validation tests for REST API endpoints and request/response handling
 */

import { z } from 'zod'
import { Assert, PerformanceTimer } from './test-utils'
import { ValidationSchemas } from '../error-handling'

interface TestResult {
  passed: boolean
  name: string
  duration: number
  error?: string
}

/**
 * Test: Swarm ID Validation
 */
async function testSwarmIdValidation(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const schema = ValidationSchemas.swarmId

    // Valid IDs
    const validIds = ['swarm-1', 'test-swarm', 'a']
    validIds.forEach((id) => {
      const result = schema.safeParse(id)
      Assert.isTrue(result.success, `Should accept valid swarm ID: ${id}`)
    })

    // Invalid IDs (too long)
    const longId = 'a'.repeat(101)
    const result = schema.safeParse(longId)
    Assert.isFalse(result.success)

    // Empty ID should fail
    const emptyResult = schema.safeParse('')
    Assert.isFalse(emptyResult.success)

    const duration = timer.end()
    return { passed: true, name: 'testSwarmIdValidation', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testSwarmIdValidation',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Agent ID Validation
 */
async function testAgentIdValidation(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const schema = ValidationSchemas.agentId

    // Valid IDs
    const validIds = ['agent-1', 'worker-001', 'scout']
    validIds.forEach((id) => {
      const result = schema.safeParse(id)
      Assert.isTrue(result.success, `Should accept valid agent ID: ${id}`)
    })

    // Invalid IDs
    const emptyResult = schema.safeParse('')
    Assert.isFalse(emptyResult.success)

    const longId = 'a'.repeat(101)
    const longResult = schema.safeParse(longId)
    Assert.isFalse(longResult.success)

    const duration = timer.end()
    return { passed: true, name: 'testAgentIdValidation', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testAgentIdValidation',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Task ID Validation
 */
async function testTaskIdValidation(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const schema = ValidationSchemas.taskId

    // Valid IDs
    const validIds = ['task-1', 'task-batch-001', 't']
    validIds.forEach((id) => {
      const result = schema.safeParse(id)
      Assert.isTrue(result.success, `Should accept valid task ID: ${id}`)
    })

    // Invalid - empty string
    const emptyResult = schema.safeParse('')
    Assert.isFalse(emptyResult.success)

    const duration = timer.end()
    return { passed: true, name: 'testTaskIdValidation', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testTaskIdValidation',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Priority Validation
 */
async function testPriorityValidation(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const schema = ValidationSchemas.priority

    // Valid priorities
    const validPriorities = ['low', 'normal', 'high', 'critical']
    validPriorities.forEach((priority) => {
      const result = schema.safeParse(priority)
      Assert.isTrue(result.success, `Should accept valid priority: ${priority}`)
    })

    // Invalid priorities
    const invalidPriorities = ['urgent', 'medium', 'CRITICAL', '']
    invalidPriorities.forEach((priority) => {
      const result = schema.safeParse(priority)
      Assert.isFalse(result.success, `Should reject invalid priority: ${priority}`)
    })

    const duration = timer.end()
    return { passed: true, name: 'testPriorityValidation', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testPriorityValidation',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Status Validation
 */
async function testStatusValidation(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const schema = ValidationSchemas.status

    // Valid statuses
    const validStatuses = [
      'idle',
      'active',
      'processing',
      'learning',
      'corrupted',
      'recovering',
      'terminated',
    ]
    validStatuses.forEach((status) => {
      const result = schema.safeParse(status)
      Assert.isTrue(result.success, `Should accept valid status: ${status}`)
    })

    // Invalid statuses
    const invalidStatuses = ['IDLE', 'activating', '']
    invalidStatuses.forEach((status) => {
      const result = schema.safeParse(status)
      Assert.isFalse(result.success, `Should reject invalid status: ${status}`)
    })

    const duration = timer.end()
    return { passed: true, name: 'testStatusValidation', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testStatusValidation',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Task Type Validation
 */
async function testTaskTypeValidation(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const schema = ValidationSchemas.taskType

    // Valid task types
    const validTypes = ['analysis', 'processing', 'optimization', 'learning']
    validTypes.forEach((type) => {
      const result = schema.safeParse(type)
      Assert.isTrue(result.success, `Should accept valid task type: ${type}`)
    })

    // Invalid task types
    const emptyResult = schema.safeParse('')
    Assert.isFalse(emptyResult.success)

    const longType = 'a'.repeat(51)
    const longResult = schema.safeParse(longType)
    Assert.isFalse(longResult.success)

    const duration = timer.end()
    return { passed: true, name: 'testTaskTypeValidation', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testTaskTypeValidation',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Message Type Validation
 */
async function testMessageTypeValidation(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const schema = ValidationSchemas.messageType

    // Valid message types
    const validTypes = ['command', 'query', 'response', 'broadcast', 'alert']
    validTypes.forEach((type) => {
      const result = schema.safeParse(type)
      Assert.isTrue(result.success, `Should accept valid message type: ${type}`)
    })

    // Invalid message types
    const invalidTypes = ['notification', 'message', '']
    invalidTypes.forEach((type) => {
      const result = schema.safeParse(type)
      Assert.isFalse(result.success, `Should reject invalid message type: ${type}`)
    })

    const duration = timer.end()
    return { passed: true, name: 'testMessageTypeValidation', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testMessageTypeValidation',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Create Swarm Request Schema
 */
async function testCreateSwarmSchema(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
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

    // Valid request
    const validRequest = {
      swarmId: 'test-swarm',
      agents: [
        { type: 'coordinator', name: 'Main Coordinator' },
        { type: 'worker', name: 'Worker 1' },
      ],
      config: { maxAgents: 100 },
    }
    const validResult = CreateSwarmSchema.safeParse(validRequest)
    Assert.isTrue(validResult.success)

    // Missing required swarmId
    const invalidRequest = {
      agents: [{ type: 'worker', name: 'Worker' }],
    }
    const invalidResult = CreateSwarmSchema.safeParse(invalidRequest)
    Assert.isFalse(invalidResult.success)

    // Invalid agent type
    const invalidAgentType = {
      swarmId: 'test',
      agents: [{ type: 'invalid', name: 'Agent' }],
    }
    const invalidAgentResult = CreateSwarmSchema.safeParse(invalidAgentType)
    Assert.isFalse(invalidAgentResult.success)

    const duration = timer.end()
    return { passed: true, name: 'testCreateSwarmSchema', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testCreateSwarmSchema',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Queue Task Request Schema
 */
async function testQueueTaskSchema(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const QueueTaskSchema = z.object({
      type: z.string(),
      priority: z.enum(['low', 'normal', 'high', 'critical']),
      goal: z.string(),
    })

    // Valid request
    const validRequest = {
      type: 'analysis',
      priority: 'high',
      goal: 'Analyze dataset',
    }
    const validResult = QueueTaskSchema.safeParse(validRequest)
    Assert.isTrue(validResult.success)

    // Invalid priority
    const invalidPriority = {
      type: 'analysis',
      priority: 'urgent',
      goal: 'Analyze',
    }
    const invalidResult = QueueTaskSchema.safeParse(invalidPriority)
    Assert.isFalse(invalidResult.success)

    // Missing required fields
    const incompletRequest = {
      type: 'analysis',
    }
    const incompleteResult = QueueTaskSchema.safeParse(incompletRequest)
    Assert.isFalse(incompleteResult.success)

    const duration = timer.end()
    return { passed: true, name: 'testQueueTaskSchema', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testQueueTaskSchema',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Consensus Request Schema
 */
async function testConsensusSchema(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const ConsensusSchema = z.object({
      proposal: z.record(z.any()),
    })

    // Valid request
    const validRequest = {
      proposal: {
        id: 'prop-1',
        action: 'update_shared_state',
        data: { key: 'value' },
      },
    }
    const validResult = ConsensusSchema.safeParse(validRequest)
    Assert.isTrue(validResult.success)

    // Invalid - missing proposal
    const invalidRequest = {
      data: { key: 'value' },
    }
    const invalidResult = ConsensusSchema.safeParse(invalidRequest)
    Assert.isFalse(invalidResult.success)

    // Valid - empty proposal object
    const emptyProposal = {
      proposal: {},
    }
    const emptyResult = ConsensusSchema.safeParse(emptyProposal)
    Assert.isTrue(emptyResult.success)

    const duration = timer.end()
    return { passed: true, name: 'testConsensusSchema', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testConsensusSchema',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Error Response Format
 */
async function testErrorResponseFormat(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const ErrorResponseSchema = z.object({
      success: z.boolean(),
      error: z.string().optional(),
      details: z.any().optional(),
    })

    // Valid error response
    const errorResponse = {
      success: false,
      error: 'Validation failed',
      details: ['Field required', 'Invalid type'],
    }
    const result = ErrorResponseSchema.safeParse(errorResponse)
    Assert.isTrue(result.success)

    // Success response (error field optional)
    const successResponse = {
      success: true,
    }
    const successResult = ErrorResponseSchema.safeParse(successResponse)
    Assert.isTrue(successResult.success)

    const duration = timer.end()
    return { passed: true, name: 'testErrorResponseFormat', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testErrorResponseFormat',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Test: Query Parameter Validation
 */
async function testQueryParameterValidation(): Promise<TestResult> {
  const timer = new PerformanceTimer()
  timer.begin()

  try {
    const QuerySchema = z.object({
      swarmId: z.string().min(1).optional(),
      action: z
        .enum(['metrics', 'report', 'health', 'agents', 'tasks', 'consensus'])
        .optional(),
      limit: z.coerce.number().int().min(1).max(1000).optional(),
    })

    // Valid query
    const validQuery = {
      swarmId: 'test-swarm',
      action: 'metrics',
      limit: 100,
    }
    const validResult = QuerySchema.safeParse(validQuery)
    Assert.isTrue(validResult.success)

    // Invalid action
    const invalidQuery = {
      action: 'invalid-action',
    }
    const invalidResult = QuerySchema.safeParse(invalidQuery)
    Assert.isFalse(invalidResult.success)

    // Valid partial query
    const partialQuery = {
      swarmId: 'test',
    }
    const partialResult = QuerySchema.safeParse(partialQuery)
    Assert.isTrue(partialResult.success)

    const duration = timer.end()
    return { passed: true, name: 'testQueryParameterValidation', duration }
  } catch (error) {
    const duration = timer.end()
    return {
      passed: false,
      name: 'testQueryParameterValidation',
      duration,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Run all API validation tests
 */
export async function runApiValidationTests() {
  const tests = await Promise.all([
    testSwarmIdValidation(),
    testAgentIdValidation(),
    testTaskIdValidation(),
    testPriorityValidation(),
    testStatusValidation(),
    testTaskTypeValidation(),
    testMessageTypeValidation(),
    testCreateSwarmSchema(),
    testQueueTaskSchema(),
    testConsensusSchema(),
    testErrorResponseFormat(),
    testQueryParameterValidation(),
  ])

  const passed = tests.filter((t) => t.passed).length

  return {
    passed,
    tests,
  }
}
