# language: en
@fail_closed @ciso @tier1 @existential
Feature: Fail-closed behavior

  As a CISO operating an enterprise code generation system
  I want ambiguous or unsafe operations to halt rather than silently proceed
  So that the system never produces unverified or unaccountable artifacts

  Background:
    Given the workspace is initialized with enterprise-strict profile
    And all policies are enforced

  @tier1 @missing_ontology
  Scenario: Missing ontology truth halts sync
    Given a required protocol-visible value "protocol_field" is absent from ontology bindings
    When the operator runs "ggen sync"
    Then sync fails
    And no artifacts are emitted

  @tier1 @ambiguous_runtime
  Scenario: Ambiguous pack resolution halts installation
    Given two legal pack resolutions satisfy the same request
    And policy requires explicit disambiguation
    When the operator runs "ggen packs install ambiguous-name"
    Then installation fails
    And the failure asks for an explicit selector

  @tier1 @unknown_compatibility
  Scenario: Unknown compatibility result halts sync
    Given compatibility evaluation cannot determine whether two packs are safe together
    When the operator runs "ggen sync"
    Then sync fails
    And the result is not reported as compatible

  @tier1 @stubbed_forbidden
  Scenario: Stubbed command behavior is forbidden in production profile
    Given a command implementation returns placeholder success or empty stub output
    And the profile is enterprise-strict
    When the operator runs the command
    Then the command fails
    And the failure states that non-authoritative or stub behavior is not allowed
