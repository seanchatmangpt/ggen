Feature: Multi-language CLI Generation
  Verify multi-language example from README

  Background:
    Given I have a clean project directory

  Scenario: Generate for multiple languages using marketplace
    When I run "ggen market add io.ggen.rust.cli-subcommand"
    And I run "ggen market add io.ggen.python.cli-subcommand"
    And I run "ggen market add io.ggen.bash.cli-subcommand"
    When I run "ggen project gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=status description='Show app status'"
    Then the file "src/cmds/status.rs" should exist
    When I run "ggen project gen io.ggen.python.cli-subcommand:cli/subcommand/python.tmpl name=status description='Show app status'"
    Then the file "commands/status.py" should exist
    When I run "ggen project gen io.ggen.bash.cli-subcommand:cli/subcommand/bash.tmpl name=status description='Show app status'"
    Then the file "commands/status.sh" should exist

  Scenario: Generate for multiple languages using local templates
    Given I have templates for Rust, Python, and Bash
    When I run "ggen project gen cli subcommand --vars cmd=status summary='Show app status'"
    Then files should be generated for all languages
    And all files should derive from the same ontology
