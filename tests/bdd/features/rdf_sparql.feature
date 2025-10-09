Feature: RDF and SPARQL Integration
  Verify RDF and SPARQL functionality

  Scenario: Load external RDF files
    Given I have RDF file "graphs/schema.ttl"
    And I have a template that references "graphs/schema.ttl"
    When I run "rgen gen test-template"
    Then the RDF data should be loaded
    And it should be available for SPARQL queries

  Scenario: Use inline RDF in Turtle format
    Given I have a template with inline Turtle RDF
    When I run "rgen gen test-template"
    Then the inline RDF should be parsed
    And it should be added to the graph

  Scenario: Execute SPARQL queries for variable extraction
    Given I have RDF data with entities
    And I have a template with SPARQL query for those entities
    When I run "rgen gen test-template"
    Then variables should be extracted via SPARQL
    And they should be available in the template

  Scenario: Use namespace prefixes
    Given I have a template with custom prefixes
    When I run "rgen gen test-template"
    Then the prefixes should be registered
    And they should be usable in RDF and SPARQL
