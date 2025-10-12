Feature: Test Scenario Execution
  As a developer
  I want to run complex test scenarios in cleanroom
  So that I can validate CLI tools and services comprehensively

  Background:
    Given I have a clean test environment

  Scenario: Simple test scenario execution
    Given I have a test scenario "simple_test"
    When I run the scenario "simple_test"
    Then the scenario should complete successfully
    And the scenario should produce output

  Scenario: Test scenario with policy
    Given I have a test scenario "policy_test"
    And I have a policy file "test_policy.yaml" with:
      """
      network: isolated
      filesystem: readonly
      memory_limit: 256MB
      """
    When I run the scenario with policy "test_policy.yaml"
    Then the scenario should complete successfully
    And the policy should be active

  Scenario: Test scenario with backend selection
    Given I have a test scenario "backend_test"
    And I have Docker available
    When I run the scenario with backend "docker"
    Then the scenario should complete successfully
    And the backend should be "docker"

  Scenario: Failing test scenario
    Given I have a failing test scenario
    When I run the scenario "failing_scenario"
    Then the scenario should fail

  Scenario: Long-running test scenario
    Given I have a long-running test scenario
    When I run the scenario "long_running_scenario"
    Then the scenario should complete successfully
    And the scenario should complete within the timeout

  Scenario: Test scenario with network access
    Given I have a test scenario with network access
    And I have network isolation enabled
    When I run the scenario "network_scenario"
    Then the scenario should respect network constraints

  Scenario: Test scenario with file operations
    Given I have a test scenario with file operations
    And I have filesystem isolation enabled
    When I run the scenario "file_ops_scenario"
    Then the scenario should respect filesystem constraints

  Scenario: Parallel scenario execution
    Given I have a test scenario "parallel_test"
    When I run the scenario in parallel
    Then the scenario should complete successfully

  Scenario: Deterministic scenario execution
    Given I have a test scenario "deterministic_test"
    When I run the scenario "deterministic_test"
    And I run the scenario "deterministic_test"
    Then the scenario should produce consistent results
