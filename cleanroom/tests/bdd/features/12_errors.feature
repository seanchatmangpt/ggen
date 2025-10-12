# features/12_errors.feature
@errors
Feature: Structured errors with context, no panics in library code
  Background:
    Given a fixture project "cli_fail"

  Scenario: Timeouts produce typed errors
    Given timeout is 50 ms
    When I run ["sleep","--ms","200"] using "local"
    Then an error "Timeout" is returned
    And error context includes { "args": ["sleep","--ms","200"], "timeout_ms": 50 }

  Scenario: Engine unavailable is a skipped test, not a failure
    Given docker is unavailable
    When I run ["--help"] using "docker"
    Then the scenario is skipped with reason "engine unavailable"

  Scenario: No panics in library code
    When I run ["panic"] using "auto"
    Then exit code is nonzero
    And error kind is "ProcessExit"
