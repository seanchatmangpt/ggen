Feature: Installation
  Verify all installation methods documented in README

  Scenario: Install from crates.io
    When I run "cargo install ggen"
    Then ggen should be installed
    And "ggen --version" should output "ggen 1.0.0"

  Scenario: Build from source
    Given I clone the repository
    When I run "cargo make build"
    Then the binary "target/debug/ggen" should exist
    And "./target/debug/ggen --version" should output "ggen 1.0.0"

  Scenario: System installation
    Given I have built from source
    When I run "cargo make install"
    Then "ggen" should be in PATH
    And "ggen --version" should work
