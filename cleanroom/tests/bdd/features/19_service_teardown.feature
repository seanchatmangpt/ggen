# features/19_service_teardown.feature
@services @teardown
Feature: Idempotent teardown of services and workspaces
  Background:
    Given a fixture project "cli_pg"

  Scenario: Teardown always succeeds
    Given a postgres service is running
    When I teardown services
    And I teardown services again
    Then both operations succeed
    And no containers remain with label "cleanroom"
