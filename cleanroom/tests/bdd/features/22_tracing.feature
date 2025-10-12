# features/22_tracing.feature
@tracing
Feature: Structured tracing for runs
  Background:
    Given a fixture project "cli_smoke"

  Scenario: Trace includes spans for prepare/run/teardown
    When I run ["--help"] with tracing enabled
    Then a trace file exists
    And spans include "prepare","run","teardown"
    And each span has duration and status
