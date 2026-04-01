# language: en
@lockfile @determinism @ciso @tier1 @existential
Feature: Lockfile determinism

  As a CISO operating an enterprise code generation system
  I want lockfiles to enforce deterministic, reproducible builds
  So that every build from the same inputs produces identical artifacts

  Background:
    Given the workspace is initialized with a lockfile
    And the marketplace contains multiple versions of packages

  @tier1 @locked_version
  Scenario: Locked install uses exact lockfile version
    Given the lockfile pins "surface-mcp" to version "1.2.3"
    And version "1.2.4" is also available in the registry
    When the operator runs "ggen packs install surface-mcp --locked"
    Then version "1.2.3" is installed
    And version "1.2.4" is ignored

  @tier1 @locked_missing
  Scenario: Locked install fails when lockfile entry is missing
    Given no lockfile entry exists for "unknown-pack"
    When the operator runs "ggen packs install unknown-pack --locked"
    Then installation fails
    And the failure states that locked resolution requires an existing lock entry

  @tier1 @locked_sync_digest
  Scenario: Locked sync fails when installed pack digest differs from lockfile
    Given the lockfile records a digest "abc123" for "surface-mcp"
    And the installed pack contents have digest "def456"
    When the operator runs "ggen sync --locked"
    Then sync fails
    And no artifacts are emitted

  @tier1 @deterministic_sync
  Scenario: Repeated locked sync produces identical artifact hashes
    Given the same ontology inputs
    And the same pack graph
    And the same lockfile
    When the operator runs "ggen sync --locked" twice
    Then the emitted artifact hashes are identical
    And the receipt hashes are identical except for permitted timestamp fields

  @tier1 @lockfile_populated
  Scenario: Lockfile is populated after successful pack installation
    Given a valid trusted pack "new-pack"
    When the operator runs "ggen packs install new-pack"
    Then the lockfile contains the pack identifier
    And the lockfile contains the resolved version
    And the lockfile contains the digest
    And the lockfile contains the registry source
