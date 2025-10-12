# features/16_freeze_outputs.feature
@snapshots @idempotent
Feature: Idempotent results across repeated runs
  Background:
    Given a fixture project "cli_smoke"

  Scenario: Re-running produces identical artifacts
    When I run ["--help"] using "auto"
    And I rerun ["--help"] using "auto"
    Then artifact digests are identical
    And stdout equals previous stdout
