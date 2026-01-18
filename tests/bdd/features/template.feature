Feature: Template Management
  As a developer
  I want to create and manage local templates
  So that I can build reusable code generation patterns

  Background:
    Given I am in a clean project directory

  Scenario: Create a new template from scratch
    When I run "ggen template new hello --output templates/hello.tmpl"
    Then the command should succeed
    And the file "templates/hello.tmpl" should exist
    And the file should contain valid YAML frontmatter
    And I should see "Template created" in output

  Scenario: Create template with interactive wizard
    When I run "ggen template new person --interactive"
    And I answer "person.txt" to "Output file pattern:"
    And I answer "name,age" to "Template variables (comma-separated):"
    Then the command should succeed
    And the template should have "to:" set to "person.txt"
    And the template should have variables "name" and "age"

  Scenario: List all available templates
    Given I have a local template "hello.tmpl"
    And I have installed the gpack "io.ggen.rust.cli-subcommand"
    When I run "ggen template list"
    Then the command should succeed
    And I should see "hello.tmpl" in output
    And I should see gpacks templates in output

  Scenario: List only local templates
    Given I have a local template "hello.tmpl"
    And I have installed the gpack "io.ggen.rust.cli-subcommand"
    When I run "ggen template list --local"
    Then the command should succeed
    And I should see "hello.tmpl" in output
    And I should not see gpack templates in output

  Scenario: List only gpack templates
    Given I have a local template "hello.tmpl"
    And I have installed the gpack "io.ggen.rust.cli-subcommand"
    When I run "ggen template list --gpacks"
    Then the command should succeed
    And I should see gpack templates in output
    And I should not see "hello.tmpl" in output

  Scenario: Show detailed template information
    Given I have a template "hello.tmpl" with content:
      """
      ---
      to: "greeting.txt"
      vars:
        name: "World"
      description: "A simple greeting template"
      ---
      Hello, {{ name }}!
      """
    When I run "ggen template show hello.tmpl"
    Then the command should succeed
    And I should see "to: greeting.txt" in output
    And I should see "name: World" in output
    And I should see "A simple greeting template" in output

  Scenario: Show template from gpack
    Given I have installed the gpack "io.ggen.rust.cli-subcommand"
    When I run "ggen template show io.ggen.rust.cli-subcommand:cli/subcommand.tmpl"
    Then the command should succeed
    And I should see template metadata

  Scenario: Lint a valid template
    Given I have a template "valid.tmpl" with content:
      """
      ---
      to: "output.txt"
      ---
      Hello, {{ name }}!
      """
    When I run "ggen template lint valid.tmpl"
    Then the command should succeed
    And I should see "Template is valid" in output

  Scenario: Lint template with schema validation
    Given I have a template "with-schema.tmpl" with content:
      """
      ---
      to: "output.txt"
      rdf_inline:
        - "@prefix ex: <http://example.org/> ."
      ---
      Content
      """
    When I run "ggen template lint with-schema.tmpl"
    Then the command should succeed
    And RDF validation should pass

  Scenario: Lint template with errors
    Given I have a template "invalid.tmpl" with content:
      """
      ---
      invalid_field: true
      ---
      """
    When I run "ggen template lint invalid.tmpl"
    Then the command should fail
    And I should see "missing required field: to" in stderr

  Scenario: Lint template with SPARQL errors
    Given I have a template "bad-sparql.tmpl" with content:
      """
      ---
      to: "output.txt"
      sparql:
        bad: "INVALID SPARQL SYNTAX"
      ---
      """
    When I run "ggen template lint bad-sparql.tmpl"
    Then the command should fail
    And I should see "SPARQL syntax error" in stderr

  Scenario: Create template from existing file
    Given I have a file "example.txt" with "Hello, World!"
    When I run "ggen template new from-file --from example.txt"
    Then the command should succeed
    And a template should be created from "example.txt"

  Scenario: List templates with pattern matching
    Given I have templates "hello.tmpl", "goodbye.tmpl", "test.tmpl"
    When I run "ggen template list --pattern '**/*hello*'"
    Then the command should succeed
    And I should see "hello.tmpl" in output
    And I should not see "goodbye.tmpl" in output

  Scenario: Show template variables and their usage
    Given I have a template "person.tmpl" with content:
      """
      ---
      to: "{{ name }}.txt"
      ---
      Name: {{ name }}
      Age: {{ age }}
      City: {{ city }}
      """
    When I run "ggen template show person.tmpl --vars"
    Then the command should succeed
    And I should see "Variables used: name, age, city" in output

  Scenario: Validate template can be rendered
    Given I have a template "hello.tmpl"
    When I run "ggen template lint hello.tmpl --dry-run --var name=Test"
    Then the command should succeed
    And I should see a preview of rendered output

  Scenario: Create template with RDF schema
    When I run "ggen template new person --rdf"
    Then the command should succeed
    And the template should have RDF frontmatter section
    And the template should have SPARQL section

  Scenario: List templates with metadata
    Given I have multiple templates with descriptions
    When I run "ggen template list --detailed"
    Then the command should succeed
    And I should see descriptions for each template

  Scenario: Check template compatibility
    Given I have a template "v1.tmpl" with "version: 1.0"
    When I run "ggen template lint v1.tmpl --check-version"
    Then the command should succeed
    And I should see compatibility information
