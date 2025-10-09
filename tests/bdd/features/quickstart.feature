Feature: Quick Start
  Verify the quick start guide from README

  Background:
    Given I have a clean project directory
    And rgen is installed

  Scenario: Generate from local template
    Given I have a template "templates/cli/subcommand/rust.tmpl"
    When I run "rgen gen cli subcommand --vars cmd=hello summary='Print a greeting'"
    Then a file should be generated
    And the output should be deterministic

  Scenario: Search marketplace
    When I run "rgen search rust cli"
    Then I should see search results
    And results should contain "cli-subcommand"

  Scenario: Install and use marketplace rpack
    When I run "rgen add io.rgen.rust.cli-subcommand"
    Then the rpack should be installed
    When I run "rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello description='Print a greeting'"
    Then a file should be generated at "src/cmds/hello.rs"
