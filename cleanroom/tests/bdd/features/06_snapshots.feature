# features/06_snapshots.feature
@snapshots
Feature: Snapshots with redaction and stability
  Background:
    Given a fixture project "cli_smoke"

  Scenario: Help snapshot is stable across backends
    When I run ["--help"] using "docker"
    And I snapshot stdout as "help-output"
    Then snapshot matches on "local" backend too
    And paths and timestamps are normalized
