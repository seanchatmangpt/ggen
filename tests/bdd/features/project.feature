Feature: Project Generation
  As a developer
  I want to generate code from templates
  So that I can quickly scaffold consistent project structures

  Background:
    Given I am in a clean project directory

  Scenario: Generate from local template with variables
    Given I have a template "hello.tmpl" with content:
      """
      ---
      to: "{{ output_file }}"
      ---
      Hello, {{ name }}!
      """
    When I run "ggen project gen hello.tmpl --var name=World --var output_file=greeting.txt"
    Then the command should succeed
    And the file "greeting.txt" should exist
    And the file "greeting.txt" should contain "Hello, World!"

  Scenario: Generate dry-run plan without applying changes
    Given I have a template "hello.tmpl" with content:
      """
      ---
      to: "greeting.txt"
      ---
      Hello, {{ name }}!
      """
    When I run "ggen project plan hello.tmpl --var name=World"
    Then the command should succeed
    And I should see "Plan:" in output
    And I should see "greeting.txt" in output
    And the file "greeting.txt" should not exist

  Scenario: Apply a previously generated plan
    Given I have a plan file "changes.plan" with:
      """
      {
        "operations": [
          {
            "type": "create",
            "path": "hello.txt",
            "content": "Hello from plan!"
          }
        ]
      }
      """
    When I run "ggen project apply changes.plan"
    Then the command should succeed
    And the file "hello.txt" should exist
    And the file "hello.txt" should contain "Hello from plan!"

  Scenario: Show diff of what generation would change
    Given I have a template "hello.tmpl" with content:
      """
      ---
      to: "greeting.txt"
      ---
      Hello, {{ name }}!
      """
    And I have an existing file "greeting.txt" with "Hello, Old!"
    When I run "ggen project diff hello.tmpl --var name=World"
    Then the command should succeed
    And I should see "--- greeting.txt" in output
    And I should see "+++ greeting.txt" in output
    And I should see "-Hello, Old!" in output
    And I should see "+Hello, World!" in output
    And the file "greeting.txt" should still contain "Hello, Old!"

  Scenario: Generate from gpack template
    Given the marketplace registry is available
    And I have installed the gpack "io.ggen.rust.cli-subcommand"
    When I run "ggen project gen io.ggen.rust.cli-subcommand:cli/subcommand.tmpl --var name=hello"
    Then the command should succeed
    And I should see "Generated" in output

  Scenario: Generate with RDF data
    Given I have a template "person.tmpl" with content:
      """
      ---
      to: "{{ name }}.txt"
      rdf_inline:
        - "@prefix ex: <http://example.org/> ."
        - "ex:{{ name }} a ex:Person ."
      sparql:
        person_type: "SELECT ?type WHERE { ?s a ?type }"
      ---
      Name: {{ name }}
      Type: {{ person_type }}
      """
    When I run "ggen project gen person.tmpl --var name=Alice"
    Then the command should succeed
    And the file "Alice.txt" should exist

  Scenario: Generate fails with missing required variable
    Given I have a template "hello.tmpl" with content:
      """
      ---
      to: "greeting.txt"
      ---
      Hello, {{ name }}!
      """
    When I run "ggen project gen hello.tmpl"
    Then the command should fail
    And I should see "missing variable" in stderr

  Scenario: Generate with deterministic seed
    Given I have a template "random.tmpl" with content:
      """
      ---
      to: "output.txt"
      determinism: 42
      ---
      Random: {{ random() }}
      """
    When I run "ggen project gen random.tmpl"
    And I run "ggen project gen random.tmpl"
    Then both outputs should be identical

  Scenario: Generate with force overwrite
    Given I have a template "hello.tmpl" with content:
      """
      ---
      to: "greeting.txt"
      force: true
      ---
      New content
      """
    And I have an existing file "greeting.txt" with "Old content"
    When I run "ggen project gen hello.tmpl"
    Then the command should succeed
    And the file "greeting.txt" should contain "New content"

  Scenario: Generate with unless_exists protection
    Given I have a template "hello.tmpl" with content:
      """
      ---
      to: "greeting.txt"
      unless_exists: true
      ---
      New content
      """
    And I have an existing file "greeting.txt" with "Old content"
    When I run "ggen project gen hello.tmpl"
    Then the command should succeed
    And the file "greeting.txt" should still contain "Old content"
