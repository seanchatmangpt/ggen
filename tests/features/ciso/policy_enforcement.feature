# language: en
@policy @enforcement @ciso @tier1 @existential
Feature: Policy profile enforcement

  As a CISO operating an enterprise code generation system
  I want policy profiles to enforce strict governance over code generation
  So that only compliant code enters production

  Background:
    Given the workspace is initialized
    And policy profiles are defined for enterprise-strict, regulated-finance, and development

  @tier1 @policy_list
  Scenario: Policy list returns all available profiles
    Given the workspace is initialized
    When the operator runs "ggen policy list"
    Then the available profiles are returned
    And the result includes enterprise-strict, regulated-finance, and development when configured

  @tier1 @policy_passes
  Scenario: Policy validate passes for compliant workspace
    Given the workspace satisfies all constraints of "enterprise-strict"
    When the operator runs "ggen policy validate --profile enterprise-strict"
    Then validation succeeds

  @tier1 @template_defaults_rejected
  Scenario: Policy validate fails when template defaults are present
    Given the workspace contains template-owned defaults
    When the operator runs "ggen policy validate --profile enterprise-strict"
    Then validation fails
    And the failure identifies the violating templates

  @tier1 @inferred_rejected
  Scenario: Policy validate fails when inferred capabilities are detected
    Given a generated capability appears without ontology-backed declaration
    When the operator runs "ggen policy validate --profile enterprise-strict"
    Then validation fails
    And the failure cites inferred capability prohibition

  @tier1 @signed_receipts_required
  Scenario: Regulated-finance requires signed receipts
    Given the active profile is regulated-finance
    And receipt signing is disabled
    When the operator runs "ggen sync"
    Then sync fails
    And the failure states that signed receipts are required

  @tier1 @development_allows_experimental
  Scenario: Development profile allows weaker trust posture than enterprise-strict
    Given a pack "experimental-ai" with experimental trust tier
    When the operator runs "ggen policy validate --profile development"
    Then validation may succeed
    But when the operator runs "ggen policy validate --profile enterprise-strict"
    Then validation fails
