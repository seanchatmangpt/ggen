# language: en
@ownership @conflicts @ciso @tier1 @existential
Feature: Ownership and conflict detection

  As a CISO operating an enterprise code generation system
  I want pack conflicts to be detected before any code is generated
  So that overlapping contributions never create ambiguous or insecure artifacts

  Background:
    Given the workspace has multiple packs installed
    And ownership classes are declared for all artifacts

  @tier1 @exclusive_conflict
  Scenario: Exclusive ownership conflict fails closed
    Given two packs claim exclusive ownership of the same artifact path "src/api.rs"
    When the operator runs "ggen sync"
    Then sync fails
    And the failure identifies both owning packs and the artifact path

  @tier1 @mergeable_succeeds
  Scenario: Mergeable ownership succeeds when merge rules are satisfied
    Given two packs contribute mergeable content to the same artifact
    And a valid merge rule exists
    When the operator runs "ggen sync"
    Then sync succeeds
    And the receipt records both contributing packs

  @tier1 @undeclared_overlap
  Scenario: Undeclared overlap fails closed
    Given two packs emit the same protocol-visible field "user.email"
    And no ownership class allows overlap
    When the operator runs "ggen sync"
    Then sync fails

  @tier1 @overlay_requires_policy
  Scenario: Overlay ownership requires explicit policy
    Given a pack attempts to overlay content owned by another pack
    And no overlay policy exists
    When the operator runs "ggen sync"
    Then sync fails

  @tier1 @runtime_conflict
  Scenario: Runtime conflicts are detected before emission
    Given two packs require incompatible runtimes "axum" and "actix"
    When the operator runs "ggen sync"
    Then sync fails before template rendering
