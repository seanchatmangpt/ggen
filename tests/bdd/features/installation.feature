Feature: Installation
  Verify all installation methods documented in README

  Scenario: Install from crates.io
    When I run "cargo install rgen"
    Then rgen should be installed
    And "rgen --version" should output "rgen 0.1.0"

  Scenario: Build from source
    Given I clone the repository
    When I run "cargo make build"
    Then the binary "target/debug/rgen" should exist
    And "./target/debug/rgen --version" should output "rgen 0.1.0"

  Scenario: System installation
    Given I have built from source
    When I run "cargo make install"
    Then "rgen" should be in PATH
    And "rgen --version" should work
