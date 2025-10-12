# features/01_unified_execution.feature
@core @unified @auto
Feature: Unified execution behaves identically local and in containers
  Background:
    Given a fixture project "cli_smoke" with binary "target/debug/cli_smoke"
    And cleanroom is configured with defaults

  Scenario Outline: Help prints with identical semantics across backends
    Given backend "<backend>" is available or test is skipped
    When I run the binary with args ["--help"] using backend "<backend>"
    Then the exit code is 0
    And stdout contains "USAGE"
    And stderr is empty
    And execution is hermetic
    And mounts are deterministic
    And clock is normalized

    Examples:
      | backend  |
      | auto     |
      | local    |
      | docker   |
      | podman   |

  Scenario: Exit codes propagate
    When I run the binary with args ["exit", "--code", "7"] using backend "auto"
    Then the exit code is 7
    And stdout is empty
    And stderr contains "exiting with 7"
