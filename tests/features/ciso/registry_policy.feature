# language: en
@trust @ciso @tier1 @existential
Feature: Registry policy enforcement

  As a CISO operating an enterprise code generation system
  I want registry access to be governed by policy
  So that unapproved public registries cannot introduce untrusted software

  Background:
    Given the workspace is initialized
    And registry configurations exist for: public, private-enterprise, and mirrored-airgapped
    And the policy registry defines which registries are allowed per profile

  @tier1 @public_blocked
  Scenario: Enterprise-strict forbids public registry resolution
    Given the active profile is enterprise-strict
    And the requested pack "public-lib" exists only in the public registry
    When the operator runs "ggen packs install public-lib"
    Then installation fails
    And the failure states that the registry source is not allowed by policy

  @tier1 @private_allowed
  Scenario: Enterprise profile allows approved private registry
    Given the active profile is enterprise-strict
    And the requested pack "approved-lib" exists in an enterprise-approved private registry
    When the operator runs "ggen packs install approved-lib"
    Then installation succeeds

  @tier1 @override_rejected
  Scenario: Explicit registry override is rejected when policy forbids it
    Given the active profile forbids public registry use
    When the operator runs "ggen packs install public-lib --registry public"
    Then installation fails
    And the override is not honored

  @tier1 @offline_missing
  Scenario: Offline mode fails when pack is not locally available
    Given the pack "uncached-pack" is not present in the local cache
    When the operator runs "ggen packs install uncached-pack --offline"
    Then installation fails
    And the failure states that offline resolution was impossible

  @tier1 @offline_cached
  Scenario: Offline mode succeeds when pack is cached and lock-resolved
    Given the pack "cached-pack" is present in the local cache
    And the lockfile pins "cached-pack" to version "1.0.0" and digest "abc123"
    When the operator runs "ggen packs install cached-pack --offline --locked"
    Then installation succeeds
    And the installed digest matches the lockfile
