Feature: RDF Graph Operations
  As a developer
  I want to directly inspect and manage the knowledge graph
  So that I can understand and debug template data flows

  Background:
    Given I am in a clean project directory

  Scenario: Load RDF data from Turtle file
    Given I have an RDF file "data.ttl" with content:
      """
      @prefix ex: <http://example.org/> .
      @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

      ex:Alice a ex:Person ;
               ex:name "Alice" ;
               ex:age 30 .
      """
    When I run "ggen graph load data.ttl"
    Then the command should succeed
    And I should see "Loaded 3 triples" in output

  Scenario: Load RDF data from multiple formats
    Given I have an RDF file "data.rdf" in RDF/XML format
    When I run "ggen graph load data.rdf --format rdfxml"
    Then the command should succeed
    And the graph should contain the triples

  Scenario: Query graph with SPARQL
    Given I have a graph with person data
    When I run "ggen graph query 'SELECT ?name WHERE { ?person ex:name ?name }'"
    Then the command should succeed
    And I should see query results in output

  Scenario: Query graph from file
    Given I have a SPARQL query file "find-people.sparql" with:
      """
      PREFIX ex: <http://example.org/>
      SELECT ?name ?age WHERE {
        ?person a ex:Person ;
                ex:name ?name ;
                ex:age ?age .
      }
      """
    And I have a graph with person data
    When I run "ggen graph query --file find-people.sparql"
    Then the command should succeed
    And I should see "Alice" and "30" in results

  Scenario: Query with different output formats
    Given I have a graph with person data
    When I run "ggen graph query 'SELECT ?name WHERE { ?p ex:name ?name }' --format json"
    Then the command should succeed
    And the output should be valid JSON

  Scenario: Query with table output
    Given I have a graph with person data
    When I run "ggen graph query 'SELECT ?name ?age WHERE { ?p ex:name ?name ; ex:age ?age }' --format table"
    Then the command should succeed
    And I should see a formatted table in output

  Scenario: Export graph to Turtle
    Given I have a graph with person data
    When I run "ggen graph export --format turtle --output export.ttl"
    Then the command should succeed
    And the file "export.ttl" should exist
    And the file should contain valid Turtle syntax

  Scenario: Export graph to JSON-LD
    Given I have a graph with person data
    When I run "ggen graph export --format jsonld --output export.jsonld"
    Then the command should succeed
    And the file should be valid JSON-LD

  Scenario: Export graph to N-Triples
    Given I have a graph with person data
    When I run "ggen graph export --format ntriples --output export.nt"
    Then the command should succeed
    And the file should contain N-Triples format

  Scenario: Load and query in single workflow
    Given I have an RDF file "data.ttl"
    When I run "ggen graph load data.ttl"
    And I run "ggen graph query 'SELECT * WHERE { ?s ?p ?o }'"
    Then I should see all triples in results

  Scenario: Query with prefixes
    Given I have a graph with person data
    When I run "ggen graph query --prefix ex=http://example.org/ 'SELECT ?name WHERE { ?p ex:name ?name }'"
    Then the command should succeed

  Scenario: Count triples in graph
    Given I have a graph with 10 triples
    When I run "ggen graph query 'SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }'"
    Then I should see "10" in results

  Scenario: Find all classes in graph
    Given I have a graph with multiple RDF types
    When I run "ggen graph query 'SELECT DISTINCT ?class WHERE { ?s a ?class }'"
    Then the command should succeed
    And I should see all unique classes

  Scenario: Find properties of a specific subject
    Given I have a graph with person data
    When I run "ggen graph query 'SELECT ?p ?o WHERE { ex:Alice ?p ?o }'"
    Then I should see all properties and values for Alice

  Scenario: Load with named graph
    Given I have an RDF file "data.ttl"
    When I run "ggen graph load data.ttl --graph http://example.org/graph1"
    Then the command should succeed
    And the triples should be in the named graph

  Scenario: Query specific named graph
    Given I have triples in named graph "http://example.org/graph1"
    When I run "ggen graph query 'SELECT * FROM <http://example.org/graph1> WHERE { ?s ?p ?o }'"
    Then I should see only triples from that graph

  Scenario: Validate graph against SHACL shapes
    Given I have a graph with person data
    And I have SHACL shapes defining person constraints
    When I run "ggen graph validate --shapes shapes.ttl"
    Then the command should succeed
    And I should see validation report

  Scenario: Show graph statistics
    Given I have a graph with 100 triples
    When I run "ggen graph stats"
    Then the command should succeed
    And I should see "Triples: 100" in output
    And I should see number of subjects
    And I should see number of predicates
    And I should see number of objects

  Scenario: Clear graph data
    Given I have a graph with person data
    When I run "ggen graph clear"
    Then the command should succeed
    And the graph should be empty

  Scenario: Merge multiple RDF files
    Given I have RDF files "data1.ttl" and "data2.ttl"
    When I run "ggen graph load data1.ttl data2.ttl"
    Then the command should succeed
    And the graph should contain triples from both files

  Scenario: Query with LIMIT and OFFSET
    Given I have a graph with 50 people
    When I run "ggen graph query 'SELECT ?name WHERE { ?p ex:name ?name } LIMIT 10 OFFSET 20'"
    Then I should see 10 results
    And they should be results 21-30

  Scenario: Export subset of graph
    Given I have a large graph
    When I run "ggen graph export --filter 'ex:Person' --output people.ttl"
    Then the command should succeed
    And the file should only contain Person entities

  Scenario: Visualize graph structure
    Given I have a graph with person relationships
    When I run "ggen graph visualize --output graph.dot"
    Then the command should succeed
    And the file should be valid DOT format
    And it can be rendered with Graphviz
