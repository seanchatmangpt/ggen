# features/18_cli_integration.feature
@cli @tools
Feature: Cargo integration via package.metadata.cleanroom
  Background:
    Given Cargo.toml contains:
      """
      [package.metadata.cleanroom]
      bin = "target/debug/cli_smoke"
      enabled = true
      """

  Scenario: One-flag CI enablement uses container backend automatically
    Given CI environment is detected
    When I run ["--help"] with backend "auto"
    Then resolved backend is not "local" if a container engine is available
    And exit code is 0
