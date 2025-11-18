/**
 * Master Test Runner
 * Executes all test suites and generates comprehensive report
 */

import { TestReporter, PerformanceTimer } from './test-utils'
import { runAgentTests } from './agent-core.test'
import { runSwarmTests } from './swarm-orchestration.test'
import { runApiValidationTests } from './api-validation.test'

/**
 * Run all tests
 */
export async function runAllTests() {
  const timer = new PerformanceTimer()
  const reporter = new TestReporter()

  console.log('\n🚀 Starting 2028+ Platform Test Suite\n')

  timer.begin()

  try {
    // Agent Core Tests
    console.log('📝 Running Agent Core Tests...')
    timer.mark('agent-tests-start')

    const agentTests = await runAgentTests()
    console.log(`✅ Agent Core: ${agentTests.passed}/${agentTests.tests.length} passed\n`)

    timer.mark('agent-tests-end')

    // Swarm Orchestration Tests
    console.log('📝 Running Swarm Orchestration Tests...')
    timer.mark('swarm-tests-start')

    const swarmTests = await runSwarmTests()
    console.log(`✅ Swarm Orchestration: ${swarmTests.passed}/${swarmTests.tests.length} passed\n`)

    timer.mark('swarm-tests-end')

    // API Validation Tests
    console.log('📝 Running API Validation Tests...')
    timer.mark('api-tests-start')

    const apiTests = await runApiValidationTests()
    console.log(`✅ API Validation: ${apiTests.passed}/${apiTests.tests.length} passed\n`)

    timer.mark('api-tests-end')

    // TODO: Add more test suites as they're created
    // - Consensus Mechanism Tests
    // - Quantum Crypto Tests
    // - Integration Tests

    // Final report
    console.log('\n' + '='.repeat(50))
    console.log('TEST EXECUTION COMPLETE')
    console.log('='.repeat(50) + '\n')

    const totalDuration = timer.end()
    const totalTests = agentTests.tests.length + swarmTests.tests.length + apiTests.tests.length
    const totalPassed = agentTests.passed + swarmTests.passed + apiTests.passed
    const passRate = ((totalPassed / totalTests) * 100).toFixed(2)

    console.log(`📊 Summary:`)
    console.log(`   Total Tests: ${totalTests}`)
    console.log(`   Passed: ${totalPassed}`)
    console.log(`   Failed: ${totalTests - totalPassed}`)
    console.log(`   Pass Rate: ${passRate}%`)
    console.log(`   Duration: ${totalDuration}ms\n`)

    // Print failed tests
    const allTests = [...agentTests.tests, ...swarmTests.tests, ...apiTests.tests]
    const failedTests = allTests.filter((t) => !t.passed)
    if (failedTests.length > 0) {
      console.log('❌ Failed Tests:')
      failedTests.forEach((test) => {
        console.log(`   - ${test.name}`)
        console.log(`     ${test.error}`)
      })
      console.log()
    }

    return {
      success: failedTests.length === 0,
      totalTests,
      passed: totalPassed,
      failed: failedTests.length,
      passRate: parseFloat(passRate),
      duration: totalDuration,
    }
  } catch (error) {
    console.error('❌ Test execution failed:', error)
    return {
      success: false,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

/**
 * Run only critical tests (fast)
 */
export async function runCriticalTests() {
  console.log('\n⚡ Running Critical Tests\n')

  const timer = new PerformanceTimer()
  timer.begin()

  const agentTests = await runAgentTests()

  const duration = timer.end()
  const failed = agentTests.tests.filter((t) => !t.passed).length

  console.log(
    `\n${failed === 0 ? '✅' : '❌'} Critical Tests: ${agentTests.passed}/${agentTests.tests.length} passed (${duration}ms)\n`
  )

  return failed === 0
}

// Execute tests when run directly
if (require.main === module || import.meta.url === `file://${process.argv[1]}`) {
  runAllTests().then((result) => {
    process.exit(result.success ? 0 : 1)
  })
}
