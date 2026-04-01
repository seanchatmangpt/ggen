# language: en
@tier2 @architectural @ciso
Feature: Template authority and no hidden semantics

  As a CISO operating an enterprise code generation system
  I want templates to be prevented from inventing protocol-visible semantics
  So that all emitted code is fully declared in the ontology

  Background:
    Given the workspace uses Tera templates
    And the system rejects legacy Handlebars templates

  @tier2 @canonical_only
  Scenario: Rendering uses only the canonical template engine
    Given the workspace contains both canonical and legacy template formats
    When the operator runs "ggen sync"
    Then only canonical templates are used for governed emission
    And legacy templates are ignored or rejected according to policy

  @tier2 @undeclared_default
  Scenario: Sync fails when a template introduces an undeclared default
    Given a template contains a defaulted protocol-visible field
    And that field is absent from ontology bindings
    When the operator runs "ggen sync"
    Then sync fails
    And the violation identifies the template and field

  @tier2 @hardcoded_capability
  Scenario: Sync fails when a template emits a hardcoded capability
    Given a template contains a capability value not present in bindings
    When the operator runs "ggen sync"
    Then sync fails
    And the failure states that templates may not invent semantics

  @tier2 @pack_scoped_resolution
  Scenario: Resolver uses pack-scoped template path deterministically
    Given a template reference uses "surface-mcp:templates/api.rs.tera"
    When the operator runs "ggen sync"
    Then the resolver loads the template from the installed pack path
    And the resolved template path appears in the receipt
