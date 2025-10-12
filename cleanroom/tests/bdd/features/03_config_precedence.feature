# features/03_config_precedence.feature
@core @config
Feature: Config precedence and validation
  Background:
    Given a fixture project "cli_smoke"
    And Cargo.toml contains:
      """
      [package.metadata.cleanroom]
      timeout_ms = 10000
      env.FROM_TOML = "1"
      """

  Scenario: Test options override toml and env
    Given environment CLEANROOM_TIMEOUT_MS="20000"
    And I pass --timeout-ms=5000 to run()
    When I run ["--help"]
    Then effective timeout is 5000
    And env contains "FROM_TOML=1"

  Scenario: Env overrides defaults but not explicit test options
    Given environment CLEANROOM_TIMEOUT_MS="15000"
    When I run ["--help"]
    Then effective timeout is 15000
