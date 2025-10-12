# features/00_basic.feature
@basic @standalone
Feature: Basic cleanroom functionality
  Background:
    Given a cleanroom project
    And cleanroom is configured with defaults

  Scenario: Basic binary execution
    Given backend "local" is available
    When I run the binary with args "--help"
    Then the exit code is 0
    And stdout contains "Mock output:"
    And stderr is empty
    And execution is hermetic
    And mounts are deterministic
    And clock is normalized
