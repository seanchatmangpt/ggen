Feature: Test Assertions and Validation
  As a developer
  I want to validate test results and artifacts
  So that I can ensure test quality and reliability

  Background:
    Given I have a clean test environment

  Scenario: Basic output validation
    Given I have expected output "Hello World"
    When I run cleanroom "echo 'Hello World'"
    Then the command should succeed
    And the output should match the expected result

  Scenario: Exit code validation
    Given I have expected exit code 0
    When I run cleanroom "echo 'test'"
    Then the command should succeed
    And the exit code should match the expected code

  Scenario: File existence validation
    Given I have expected files: "output.txt,result.json"
    When I run cleanroom "echo 'test' > output.txt && echo '{\"status\":\"ok\"}' > result.json"
    Then the command should succeed
    And all expected files should exist

  Scenario: Artifact validation
    Given I have expected artifacts: "binary.bin,data.dat"
    When I run cleanroom "dd if=/dev/zero of=binary.bin bs=1024 count=1 && echo 'data' > data.dat"
    Then the command should succeed
    And all expected artifacts should be present

  Scenario: Deterministic results validation
    Given I have a test scenario "deterministic_test"
    When I run the scenario "deterministic_test"
    And I run the scenario "deterministic_test"
    Then the results should be deterministic

  Scenario: Reproducible results validation
    Given I have a test scenario "reproducible_test"
    When I run the scenario "reproducible_test"
    Then the results should be reproducible

  Scenario: Isolation validation
    Given I have network isolation enabled
    And I have filesystem isolation enabled
    When I run cleanroom "echo 'isolated test'"
    Then the command should succeed
    And the execution should be isolated

  Scenario: Security constraints validation
    Given I have a security policy with:
      """
      network: blocked
      filesystem: readonly
      memory_limit: 128MB
      """
    When I run cleanroom "echo 'security test'"
    Then the command should succeed
    And the security constraints should be enforced

  Scenario: Performance validation
    Given I have resource limits configured
    When I run cleanroom "echo 'performance test'"
    Then the command should succeed
    And the performance should be within limits

  Scenario: Assertion file validation
    Given I have an assertion file "test_assertions.txt" with:
      """
      assert_output_contains: success
      assert_file_exists: output.txt
      """
    When I run assertions from "test_assertions.txt"
    And I run cleanroom "echo 'success' > output.txt"
    Then the command should succeed
    And the assertions should all pass

  Scenario: Baseline comparison
    Given I have a file "baseline.txt" with content:
      """
      Expected output
      Test results
      """
    When I compare with baseline "baseline.txt"
    And I run cleanroom "echo 'Expected output'; echo 'Test results'"
    Then the command should succeed
    And the baseline comparison should match
