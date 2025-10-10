Feature: Deterministic Generation
  Verify determinism claims from README

  Scenario: Same inputs produce identical outputs
    Given I have a template with seed "test-seed"
    And I have RDF graph data
    When I run "ggen project gen test-template" with seed "test-seed"
    And I capture the output hash
    When I run "ggen project gen test-template" with seed "test-seed" again
    And I capture the second output hash
    Then both output hashes should be identical

  Scenario: Different seeds produce different outputs
    Given I have a template
    When I run "ggen project gen test-template" with seed "seed1"
    And I capture the first output
    When I run "ggen project gen test-template" with seed "seed2"
    And I capture the second output
    Then the outputs should be different

  Scenario: Manifest hash computation
    Given I have a template with:
      """
      ---
      to: output/{{ name }}.txt
      vars:
        name: world
      determinism:
        seed: "test-seed"
      ---
      Hello, {{ name }}!
      """
    When I run "ggen project gen test-template"
    Then a manifest hash should be computed
    And the same inputs should produce the same manifest hash
