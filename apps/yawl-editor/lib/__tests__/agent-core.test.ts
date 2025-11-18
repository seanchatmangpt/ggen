/**
 * Tests for Agent Core System
 */

import { Agent, AgentStatus, CoordinatorAgent, WorkerAgent, ScoutAgent, LearningAgent } from '../agents/agent-core'
import { Assert, Fixtures, runTestSuite, PerformanceTimer } from './test-utils'

/**
 * Agent Creation & Lifecycle Tests
 */
async function testAgentCreation() {
  const agent = new Agent('test-1', 'Test Agent', 'worker')

  Assert.equals(agent.getId(), 'test-1')
  Assert.equals(agent.getName(), 'Test Agent')
  Assert.equals(agent.getRole(), 'worker')
  Assert.equals(agent.getStatus(), AgentStatus.IDLE)
}

async function testAgentStatusTransition() {
  const agent = new Agent('test-2', 'Test Agent', 'worker')

  agent.setStatus(AgentStatus.ACTIVE)
  Assert.equals(agent.getStatus(), AgentStatus.ACTIVE)

  agent.setStatus(AgentStatus.PROCESSING)
  Assert.equals(agent.getStatus(), AgentStatus.PROCESSING)

  agent.setStatus(AgentStatus.IDLE)
  Assert.equals(agent.getStatus(), AgentStatus.IDLE)
}

/**
 * Capability Tests
 */
async function testCapabilityManagement() {
  const agent = new Agent('test-3', 'Test Agent', 'worker')

  Assert.equals(agent.getCapabilities().length, 0)

  agent.addCapability({
    name: 'task_execution',
    description: 'Execute tasks',
    version: '1.0',
    performanceRating: 85,
    energyCost: 10,
  })

  Assert.equals(agent.getCapabilities().length, 1)
  Assert.isTrue(agent.hasCapability('task_execution'))
  Assert.equals(agent.getCapabilityRating('task_execution'), 85)

  agent.removeCapability('task_execution')
  Assert.equals(agent.getCapabilities().length, 0)
  Assert.isFalse(agent.hasCapability('task_execution'))
}

/**
 * Knowledge Management Tests
 */
async function testKnowledgeManagement() {
  const agent = new Agent('test-4', 'Test Agent', 'learner')

  Assert.equals(agent.getKnowledgeSize(), 0)

  agent.addKnowledge({
    id: 'know-1',
    domain: 'optimization',
    topic: 'pso',
    confidence: 0.95,
    source: 'learned',
    timestamp: new Date(),
  })

  Assert.equals(agent.getKnowledgeSize(), 1)

  const knowledge = agent.getKnowledge('know-1')
  Assert.isDefined(knowledge)
  if (knowledge) {
    Assert.equals(knowledge.domain, 'optimization')
    Assert.equals(knowledge.confidence, 0.95)
  }

  const byDomain = agent.getKnowledgeByDomain('optimization')
  Assert.arrayLength(byDomain, 1)
}

/**
 * Task Management Tests
 */
async function testTaskManagement() {
  const agent = new Agent('test-5', 'Test Agent', 'worker')

  const task = agent.createTask({
    type: 'analysis',
    priority: 'high',
    goal: 'Analyze data',
  })

  Assert.isDefined(task.id)
  Assert.equals(task.status, 'pending')
  Assert.equals(task.type, 'analysis')

  agent.executeTask(task.id)
  Assert.equals(agent.getStatus(), AgentStatus.PROCESSING)

  agent.completeTask(task.id, { result: 'success' })
  Assert.equals(agent.getStatus(), AgentStatus.IDLE)

  const completedTask = agent.getTasks({ status: 'completed' })
  Assert.arrayLength(completedTask, 1)
}

/**
 * Energy & Health Management Tests
 */
async function testEnergyManagement() {
  const agent = new Agent('test-6', 'Test Agent', 'worker')

  Assert.equals(agent.getEnergyLevel(), 100)

  agent.consumeEnergy(30)
  Assert.equals(agent.getEnergyLevel(), 70)

  agent.restoreEnergy(20)
  Assert.equals(agent.getEnergyLevel(), 90)

  agent.recharge()
  Assert.equals(agent.getEnergyLevel(), 100)
}

async function testHealthManagement() {
  const agent = new Agent('test-7', 'Test Agent', 'worker')

  Assert.equals(agent.getHealthScore(), 100)

  agent.takeDamage(30)
  Assert.equals(agent.getHealthScore(), 70)

  agent.heal(20)
  Assert.equals(agent.getHealthScore(), 90)
}

/**
 * Reputation System Tests
 */
async function testReputationSystem() {
  const agent = new Agent('test-8', 'Test Agent', 'worker')

  Assert.equals(agent.getReputation(), 50)

  agent.adjustReputation(20)
  Assert.equals(agent.getReputation(), 70)

  agent.adjustReputation(-30)
  Assert.equals(agent.getReputation(), 40)

  // Test bounds
  agent.adjustReputation(100)
  Assert.equals(agent.getReputation(), 100)

  agent.adjustReputation(-200)
  Assert.equals(agent.getReputation(), 0)
}

/**
 * Specialized Agent Tests
 */
async function testCoordinatorAgent() {
  const coordinator = new CoordinatorAgent('coord-1', 'Main Coordinator')

  Assert.equals(coordinator.getRole(), 'coordinator')
  Assert.isTrue(coordinator.hasCapability('task_distribution'))
  Assert.isTrue(coordinator.hasCapability('conflict_resolution'))
}

async function testWorkerAgent() {
  const worker = new WorkerAgent('worker-1', 'Processing Worker', 'image-analysis')

  Assert.equals(worker.getRole(), 'worker')
  Assert.isTrue(worker.hasCapability('task_execution'))
}

async function testScoutAgent() {
  const scout = new ScoutAgent('scout-1', 'Network Scout')

  Assert.equals(scout.getRole(), 'scout')
  Assert.isTrue(scout.hasCapability('exploration'))
  Assert.isTrue(scout.hasCapability('reconnaissance'))
}

async function testLearningAgent() {
  const learner = new LearningAgent('learner-1', 'AI Learner')

  Assert.equals(learner.getRole(), 'learner')
  Assert.isTrue(learner.hasCapability('machine_learning'))
  Assert.isTrue(learner.hasCapability('knowledge_synthesis'))
}

/**
 * Event System Tests
 */
async function testEventSystem() {
  const agent = new Agent('test-9', 'Test Agent', 'worker')

  let statusChangedCalled = false
  agent.on('statusChanged', () => {
    statusChangedCalled = true
  })

  agent.setStatus(AgentStatus.ACTIVE)
  Assert.isTrue(statusChangedCalled)
}

/**
 * Metrics & Serialization Tests
 */
async function testMetricsCollection() {
  const agent = new Agent('test-10', 'Test Agent', 'worker')

  // Complete some tasks
  for (let i = 0; i < 5; i++) {
    const task = agent.createTask({
      type: 'test',
      priority: 'normal',
      goal: 'Test',
    })
    agent.executeTask(task.id)
    agent.completeTask(task.id)
  }

  const metrics = agent.getMetrics()

  Assert.equals(metrics.completedTasks, 5)
  Assert.equals(metrics.failedTasks, 0)
  Assert.equals(metrics.successRate, 1.0)
  Assert.inRange(metrics.successRate, 0, 1)
}

/**
 * Run all agent tests
 */
export async function runAgentTests() {
  return runTestSuite('Agent Core System', [
    ['Agent Creation', testAgentCreation],
    ['Agent Status Transition', testAgentStatusTransition],
    ['Capability Management', testCapabilityManagement],
    ['Knowledge Management', testKnowledgeManagement],
    ['Task Management', testTaskManagement],
    ['Energy Management', testEnergyManagement],
    ['Health Management', testHealthManagement],
    ['Reputation System', testReputationSystem],
    ['Coordinator Agent', testCoordinatorAgent],
    ['Worker Agent', testWorkerAgent],
    ['Scout Agent', testScoutAgent],
    ['Learning Agent', testLearningAgent],
    ['Event System', testEventSystem],
    ['Metrics Collection', testMetricsCollection],
  ])
}
