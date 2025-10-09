Feature: Marketplace
  Verify all marketplace functionality from README

  Background:
    Given I have a clean project directory
    And the marketplace registry is available at "https://raw.githubusercontent.com/seanchatmangpt/rgen/master/registry/"

  Scenario: Search by language
    When I run "rgen search rust cli"
    Then I should see results for Rust CLI templates
    When I run "rgen search python api"
    Then I should see results for Python API templates
    When I run "rgen search typescript react"
    Then I should see results for TypeScript React templates

  Scenario: Browse categories
    When I run "rgen categories"
    Then I should see popular categories

  Scenario: Get package details
    When I run "rgen show io.rgen.rust.cli-subcommand"
    Then I should see package metadata
    And I should see version information
    And I should see description

  Scenario: Install latest version
    When I run "rgen add io.rgen.rust.cli-subcommand"
    Then the package should be installed
    And "rgen packs" should list "io.rgen.rust.cli-subcommand"

  Scenario: Install specific version
    When I run "rgen add io.rgen.rust.cli-subcommand@0.2.0"
    Then version "0.2.0" should be installed

  Scenario: Update packages
    Given I have installed "io.rgen.rust.cli-subcommand@0.1.0"
    When I run "rgen update"
    Then the package should be updated to latest version

  Scenario: Use installed rpack
    Given I have installed "io.rgen.rust.cli-subcommand"
    When I run "rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=Users"
    Then a file should be generated
    And the file should use the rpack template
