# features/26_time_budget.feature
@timing
Feature: Time budgets per step and scenario
  Background:
    Given a fixture project "cli_sleep"

  Scenario: Per-step timeout enforced
    Given step timeout is 100 ms
    When I run scenario with a step ["sleep","--ms","300"] using "auto"
    Then that step fails with timeout
    And subsequent steps still run if configured "continue_on_fail"
