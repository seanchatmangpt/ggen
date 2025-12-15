Feature: Template Generation
  Verify template structure and rendering from README

  Background:
    Given I have a clean project directory

  Scenario: Basic template with frontmatter
    Given I have a template with:
      """
      ---
      to: src/cmds/{{ slug }}.rs
      vars: { cmd: hello, summary: "Print a greeting" }
      ---
      pub fn {{ slug }}(name: &str) {
          println!("hello {}", name);
      }
      """
    When I run "ggen project gen test-template"
    Then the file "src/cmds/hello.rs" should exist
    And it should contain "pub fn hello(name: &str)"

  Scenario: Template with inline RDF
    Given I have a template with RDF inline:
      """
      ---
      to: src/cmds/{{ slug }}.rs
      vars: { cmd: hello, summary: "Print a greeting" }
      rdf:
        inline:
          - mediaType: text/turtle
            text: |
              @prefix cli: <urn:ggen:cli#> .
              [] a cli:Command ;
                 cli:slug "{{ cmd }}" ;
                 cli:summary "{{ summary }}" .
      ---
      pub fn {{ slug }}(name: &str) { }
      """
    When I run "ggen project gen test-template"
    Then the RDF graph should be processed
    And the output should use RDF-extracted variables

  Scenario: Template with SPARQL
    Given I have a template with SPARQL query:
      """
      ---
      sparql:
        vars:
          - name: slug
            query: |
              PREFIX cli: <urn:ggen:cli#>
              SELECT ?slug WHERE { ?c a cli:Command ; cli:slug ?slug } LIMIT 1
      ---
      pub fn {{ slug }}() { }
      """
    When I run "ggen project gen test-template"
    Then SPARQL variables should be extracted
    And the output should use queried values

  Scenario: Template with determinism seed
    Given I have a template with determinism config:
      """
      ---
      determinism: { seed: "{{ cmd }}" }
      ---
      """
    When I run "ggen project gen test-template" multiple times
    Then all outputs should be byte-identical
