# language: en
@trust @ciso @tier1 @existential
Feature: Trust tier enforcement

  As a CISO operating an enterprise code generation system
  I want packs to be validated against trust tier policies before installation
  So that only approved software enters the production pipeline

  Background:
    Given the workspace is initialized
    And the policy registry contains profiles for enterprise-strict, regulated-finance, and development
    And the trust registry contains pack classifications: enterprise-certified, enterprise-approved, experimental, and blocked

  @tier1 @signed_pack
  Scenario: Enterprise-approved pack installs successfully
    Given a pack "surface-mcp" signed by a trusted key
    And the pack is classified as enterprise-approved
    And the active profile allows enterprise-approved packs
    When the operator runs "ggen packs install surface-mcp"
    Then the pack is installed
    And the lockfile records the pack version, digest, and trust tier
    And the install receipt records signature verification success

  @tier1 @blocked_pack
  Scenario: Blocked pack is rejected before installation
    Given a pack "malicious-pack" is classified as blocked
    When the operator runs "ggen packs install malicious-pack"
    Then installation fails
    And no files are written to the installed pack directory
    And no lockfile entry is created
    And the failure explains that the trust tier is blocked

  @tier1 @experimental_rejected
  Scenario: Experimental pack is rejected under enterprise-strict
    Given a pack "experimental-ai" is classified as experimental
    And the active profile is enterprise-strict
    When the operator runs "ggen packs install experimental-ai"
    Then installation fails
    And the failure cites profile trust policy

  @tier1 @experimental_allowed
  Scenario: Experimental pack is allowed under development
    Given a pack "experimental-ai" is classified as experimental
    And the active profile is development
    When the operator runs "ggen packs install experimental-ai"
    Then installation succeeds
    And the lockfile records the trust tier as experimental

  @tier1 @signature_required
  Scenario: Missing signature causes hard failure
    Given a pack "unsigned-pack" with no signature metadata
    When the operator runs "ggen packs install unsigned-pack"
    Then installation fails
    And the failure states that signature verification could not be performed

  @tier1 @signature_invalid
  Scenario: Invalid signature causes hard failure
    Given a pack "tampered-pack" whose signature does not match its payload
    When the operator runs "ggen packs install tampered-pack"
    Then installation fails
    And the failure states that signature verification failed
    And no pack is installed

  @tier1 @checksum_mismatch
  Scenario: Checksum mismatch causes hard failure
    Given a pack "corrupted-pack" whose checksum does not match the published digest
    When the operator runs "ggen packs install corrupted-pack"
    Then installation fails
    And the failure states that integrity verification failed
