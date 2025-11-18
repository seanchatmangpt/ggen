/**
 * Testing Utilities for 2028+ Platform
 * Provides mocking, fixtures, and assertion helpers
 */

/**
 * Test result
 */
export interface TestResult {
  name: string
  passed: boolean
  duration: number
  error?: string
  details?: any
}

/**
 * Test suite
 */
export interface TestSuite {
  name: string
  tests: TestResult[]
  passed: number
  failed: number
  duration: number
}

/**
 * Test reporter
 */
export class TestReporter {
  private suites: TestSuite[] = []
  private currentSuite: TestResult[] = []
  private currentSuiteName: string = ''

  startSuite(name: string): void {
    this.currentSuiteName = name
    this.currentSuite = []
  }

  addTest(result: TestResult): void {
    this.currentSuite.push(result)
  }

  endSuite(): TestSuite {
    const passed = this.currentSuite.filter((t) => t.passed).length
    const duration = this.currentSuite.reduce((sum, t) => sum + t.duration, 0)

    const suite: TestSuite = {
      name: this.currentSuiteName,
      tests: this.currentSuite,
      passed,
      failed: this.currentSuite.length - passed,
      duration,
    }

    this.suites.push(suite)
    this.currentSuite = []
    return suite
  }

  getReport() {
    const totalTests = this.suites.reduce((sum, s) => sum + s.tests.length, 0)
    const totalPassed = this.suites.reduce((sum, s) => sum + s.passed, 0)
    const totalDuration = this.suites.reduce((sum, s) => sum + s.duration, 0)

    return {
      suites: this.suites.length,
      tests: totalTests,
      passed: totalPassed,
      failed: totalTests - totalPassed,
      duration: totalDuration,
      passRate: totalTests > 0 ? ((totalPassed / totalTests) * 100).toFixed(2) + '%' : '0%',
      suiteDetails: this.suites,
    }
  }

  printReport(): void {
    const report = this.getReport()

    console.log('\n=== Test Report ===')
    console.log(`Suites: ${report.suites}`)
    console.log(`Tests: ${report.passed}/${report.tests} passed`)
    console.log(`Duration: ${report.duration}ms`)
    console.log(`Pass Rate: ${report.passRate}\n`)

    report.suiteDetails.forEach((suite) => {
      const status = suite.failed === 0 ? '✅' : '❌'
      console.log(`${status} ${suite.name}: ${suite.passed}/${suite.tests.length}`)

      suite.tests.forEach((test) => {
        if (!test.passed) {
          console.log(`   ❌ ${test.name}: ${test.error}`)
        }
      })
    })
  }
}

/**
 * Assertion helpers
 */
export class Assert {
  static equals(actual: any, expected: any, message?: string): void {
    if (actual !== expected) {
      throw new Error(
        `Assertion failed: ${message || ''}\nExpected: ${expected}\nActual: ${actual}`
      )
    }
  }

  static deepEquals(actual: any, expected: any, message?: string): void {
    const actualStr = JSON.stringify(actual)
    const expectedStr = JSON.stringify(expected)

    if (actualStr !== expectedStr) {
      throw new Error(
        `Assertion failed: ${message || ''}\nExpected: ${expectedStr}\nActual: ${actualStr}`
      )
    }
  }

  static isTrue(value: boolean, message?: string): void {
    if (value !== true) {
      throw new Error(`Assertion failed: ${message || 'Expected true'}`)
    }
  }

  static isFalse(value: boolean, message?: string): void {
    if (value !== false) {
      throw new Error(`Assertion failed: ${message || 'Expected false'}`)
    }
  }

  static isDefined(value: any, message?: string): void {
    if (value === undefined || value === null) {
      throw new Error(`Assertion failed: ${message || 'Expected defined value'}`)
    }
  }

  static isNull(value: any, message?: string): void {
    if (value !== null) {
      throw new Error(`Assertion failed: ${message || 'Expected null'}`)
    }
  }

  static throws(fn: () => void, message?: string): void {
    try {
      fn()
      throw new Error(`Assertion failed: ${message || 'Expected function to throw'}`)
    } catch (error) {
      // Expected
    }
  }

  static doesNotThrow(fn: () => void, message?: string): void {
    try {
      fn()
    } catch (error) {
      throw new Error(`Assertion failed: ${message || 'Expected function not to throw'}\n${error}`)
    }
  }

  static arrayContains(array: any[], element: any, message?: string): void {
    if (!array.includes(element)) {
      throw new Error(
        `Assertion failed: ${message || ''}\nArray does not contain element: ${element}`
      )
    }
  }

  static arrayLength(array: any[], length: number, message?: string): void {
    if (array.length !== length) {
      throw new Error(
        `Assertion failed: ${message || ''}\nExpected length: ${length}, Actual: ${array.length}`
      )
    }
  }

  static inRange(value: number, min: number, max: number, message?: string): void {
    if (value < min || value > max) {
      throw new Error(
        `Assertion failed: ${message || ''}\nExpected value between ${min} and ${max}, got ${value}`
      )
    }
  }
}

/**
 * Mocks and fixtures
 */
export class Fixtures {
  static createMockAgent() {
    return {
      id: `agent-test-${Math.random()}`,
      name: 'Test Agent',
      role: 'worker',
      status: 'idle',
      energy: 100,
      health: 100,
      reputation: 50,
    }
  }

  static createMockSwarmConfig() {
    return {
      maxAgents: 10,
      consensusAlgorithm: 'pbft',
      selfHealingEnabled: true,
      learningEnabled: true,
      energyManagement: true,
    }
  }

  static createMockTask() {
    return {
      type: 'test',
      priority: 'normal',
      goal: 'Test task',
      estimatedDuration: 1000,
      reward: 5,
    }
  }

  static createMockMessage() {
    return {
      type: 'test',
      to: 'agent-1',
      content: { test: true },
      priority: 5,
    }
  }

  static createMockQuantumKey() {
    return {
      publicKey: 'test-public-key-' + Math.random(),
      privateKey: 'test-private-key-' + Math.random(),
      keyId: 'key-' + Math.random(),
      algorithm: 'hybrid-pq-2048',
      createdAt: new Date(),
    }
  }
}

/**
 * Test runner
 */
export async function runTest(
  name: string,
  fn: () => void | Promise<void>
): Promise<TestResult> {
  const startTime = Date.now()

  try {
    await fn()
    return {
      name,
      passed: true,
      duration: Date.now() - startTime,
    }
  } catch (error) {
    return {
      name,
      passed: false,
      duration: Date.now() - startTime,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Run test suite
 */
export async function runTestSuite(
  name: string,
  tests: Array<[string, () => void | Promise<void>]>
): Promise<TestSuite> {
  const reporter = new TestReporter()
  reporter.startSuite(name)

  for (const [testName, testFn] of tests) {
    const result = await runTest(testName, testFn)
    reporter.addTest(result)
  }

  return reporter.endSuite()
}

/**
 * Performance timer
 */
export class PerformanceTimer {
  private start: number = 0
  private marks: Map<string, number> = new Map()

  begin(): void {
    this.start = Date.now()
  }

  mark(label: string): void {
    this.marks.set(label, Date.now() - this.start)
  }

  end(): number {
    return Date.now() - this.start
  }

  getReport() {
    return {
      total: this.end(),
      marks: Array.from(this.marks.entries()).map(([label, time]) => ({
        label,
        time,
      })),
    }
  }

  print(): void {
    const report = this.getReport()
    console.log(`\nPerformance Report:`)
    console.log(`Total: ${report.total}ms`)
    report.marks.forEach(({ label, time }) => {
      console.log(`  ${label}: ${time}ms`)
    })
  }
}

/**
 * Data generator for load testing
 */
export class DataGenerator {
  static generateAgents(count: number) {
    const types = ['coordinator', 'worker', 'scout', 'learner']
    return Array(count)
      .fill(0)
      .map((_, i) => ({
        type: types[i % types.length],
        name: `Agent ${i}`,
        options: i % 2 === 0 ? { specialization: 'analysis' } : undefined,
      }))
  }

  static generateTasks(count: number) {
    const types = ['analysis', 'processing', 'learning', 'optimization']
    const priorities = ['low', 'normal', 'high', 'critical']

    return Array(count)
      .fill(0)
      .map((_, i) => ({
        type: types[i % types.length],
        priority: priorities[i % priorities.length],
        goal: `Task ${i}`,
        estimatedDuration: Math.random() * 5000 + 1000,
        reward: Math.random() * 20 + 5,
      }))
  }

  static generateSwarms(count: number) {
    return Array(count)
      .fill(0)
      .map((_, i) => ({
        swarmId: `swarm-${i}`,
        agents: this.generateAgents(5),
      }))
  }
}
