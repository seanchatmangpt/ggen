# features/14_concurrency.feature
@concurrency
Feature: Concurrent steps with isolation and bounded output
  Background:
    Given a fixture project "cli_workflow"

  Scenario: Two steps run concurrently without interference
    When I define concurrent scenario "dual-help"
      | step   | args       | expect  | max_output |
      | help1  | ["--help"] | success | 64KB       |
      | help2  | ["--help"] | success | 64KB       |
    And I execute the scenario on backend "auto"
    Then both steps succeeded
    And total duration < 2x single-step duration
    And per-step output <= max_output
