Feature: Cleanroom Core Functionality
  As a developer
  I want to run commands in a hermetic, deterministic environment
  So that I can test CLI tools and services with confidence

  Background:
    Given I have a clean test environment

  Scenario: Basic command execution
    Given I have a local execution environment
    When I run cleanroom "echo 'Hello World'"
    Then the command should succeed
    And I should see "Hello World" in output

  Scenario: Command execution with Docker backend
    Given I have Docker available
    When I select the "docker" backend
    And I run cleanroom "echo 'Hello from Docker'"
    Then the command should succeed
    And the backend should be "docker"
    And I should see "Hello from Docker" in output

  Scenario: Command execution with Podman backend
    Given I have Podman available
    When I select the "podman" backend
    And I run cleanroom "echo 'Hello from Podman'"
    Then the command should succeed
    And the backend should be "podman"
    And I should see "Hello from Podman" in output

  Scenario: Auto backend detection
    When I select the "auto" backend
    Then the backend should be automatically detected

  Scenario: Command failure handling
    Given I have a local execution environment
    When I run cleanroom "false"
    Then the command should fail
    And the exit code should be 1

  Scenario: Command timeout
    Given I have a local execution environment
    When I run cleanroom "sleep 10" with timeout "5s"
    Then the scenario should timeout

  Scenario: Network isolation
    Given I have network isolation enabled
    When I run cleanroom "curl -s http://httpbin.org/get"
    Then the command should fail
    And the network should be isolated

  Scenario: Filesystem isolation
    Given I have filesystem isolation enabled
    When I run cleanroom "touch /tmp/isolated_test"
    Then the command should fail
    And the filesystem should be isolated

  Scenario: Resource limits
    Given I have resource limits configured
    When I run cleanroom "echo 'Resource limited'"
    Then the command should succeed
    And the memory limit should be "512MB"
    And the CPU limit should be "1.0"
    And the timeout should be "30s"
