# features/20_artifact_contract.feature
@artifacts
Feature: Artifact capture, structure, and size limits
  Background:
    Given a fixture project "cli_artifacts"

  Scenario: Logs and files captured under run id
    When I run ["produce-artifacts"] using "auto"
    Then an artifact directory exists "artifacts/<run_id>/"
    And contains files:
      | path               |
      | stdout.log         |
      | stderr.log         |
      | config.json        |
    And total size <= 10MB by default
