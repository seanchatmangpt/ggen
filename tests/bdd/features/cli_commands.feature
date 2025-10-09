Feature: CLI Commands
  Verify all commands from README commands table

  Background:
    Given I have a clean project directory
    And rgen is installed

  Scenario: Marketplace commands work
    When I run "rgen search rust"
    Then the command should succeed
    When I run "rgen categories"
    Then the command should succeed
    When I run "rgen packs"
    Then the command should succeed

  Scenario: Generation commands work
    Given I have templates in "templates/"
    When I run "rgen list"
    Then I should see available templates
    When I run "rgen show templates/test.tmpl"
    Then I should see template metadata

  Scenario: Validation commands work
    Given I have a template
    When I run "rgen lint templates/test.tmpl"
    Then the command should validate the template

  Scenario: Utility commands work
    When I run "rgen hazard"
    Then I should see a hazard report
    When I run "rgen completion bash"
    Then I should see bash completion script
    When I run "rgen completion zsh"
    Then I should see zsh completion script
    When I run "rgen completion fish"
    Then I should see fish completion script
