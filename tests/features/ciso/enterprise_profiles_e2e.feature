# language: en
@tier3 @e2e @enterprise @ciso
Feature: Enterprise profiles end-to-end

  As a CISO operating an enterprise code generation system
  I want end-to-end workflows that demonstrate enterprise-grade governance
  So that I have confidence the system enforces all policies in production scenarios

  @tier3 @enterprise_strict_success
  Scenario: Enterprise-strict capability enable to locked sync with signed receipts
    Given the active profile is enterprise-strict
    And only enterprise-approved registries are configured
    And the requested capability surface is "mcp"
    And the projection is "rust"
    And the runtime is "axum"
    When the operator runs "ggen capability enable --surface mcp --projection rust --runtime axum --profile enterprise-strict"
    And the operator runs "ggen sync --locked"
    Then the capability resolves to approved packs only
    And the lockfile records the exact graph
    And sync succeeds through the authoritative compiler pipeline
    And artifacts are emitted
    And a signed composition receipt is emitted
    And receipt verification succeeds

  @tier3 @regulated_finance_failure
  Scenario: Regulated-finance rejects unapproved public dependency and unsigned receipt path
    Given the active profile is regulated-finance
    And a required dependency is available only from a public registry
    And receipt signing is disabled
    When the operator runs "ggen sync"
    Then sync fails
    And the failure reports both registry trust violation and receipt policy violation
    And no artifacts are emitted
