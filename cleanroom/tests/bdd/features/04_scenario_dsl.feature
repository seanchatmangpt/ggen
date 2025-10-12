# features/04_scenario_dsl.feature
@dsl
Feature: Scenario DSL for multi-step workflows
  Background:
    Given a fixture project "cli_workflow"

  Scenario: Sequential steps succeed
    When I define scenario "Build process"
      | step         | args                  | expect  |
      | version      | ["--version"]         | success |
      | build prod   | ["build","--prod"]    | success |
    And I execute the scenario on backend "auto"
    Then all steps succeeded
    And aggregated duration <= 30000 ms
    And step order is deterministic

  Scenario: Concurrent steps with deterministic aggregation
    When I define concurrent scenario "Parallel smoke"
      | step     | args             | expect  |
      | help1    | ["--help"]       | success |
      | help2    | ["--help"]       | success |
    And I execute the scenario on backend "auto"
    Then all steps succeeded
    And logs are order-stable by (start_ts, step_name)
