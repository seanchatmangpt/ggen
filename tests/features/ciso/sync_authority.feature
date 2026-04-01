# language: en
@tier2 @architectural @ciso
Feature: Compiler authority and sync integration

  As a CISO operating an enterprise code generation system
  I want the sync command to be the single authoritative code generation path
  So that all generated artifacts are governed and receipt-backed

  Background:
    Given the workspace has installed packs
    And the packs define ontology, queries, templates, and validators

  @tier2 @packs_load
  Scenario: Sync loads pack contributions into the compiler pipeline
    Given installed packs contribute ontology, queries, templates, and validators
    When the operator runs "ggen sync"
    Then the compiler loads the pack ontology
    And the compiler executes pack queries
    And the compiler renders pack templates
    And the compiler runs pack validators

  @tier2 @uninstalled_ignored
  Scenario: Pack templates are ignored unless installed through the authoritative path
    Given an uninstalled template exists outside the governed pack install path
    When the operator runs "ggen sync"
    Then the template is not used
    And the sync receipt does not reference it

  @tier2 @bridge_broken
  Scenario: Sync fails when pack-to-sync bridge is broken
    Given a required installed pack is not visible to the sync resolver
    When the operator runs "ggen sync"
    Then sync fails
    And the failure states that required pack contributions could not be loaded

  @tier2 @sync_only_path
  Scenario: Sync is the only path that emits governed artifacts
    Given a command attempts direct artifact generation outside sync
    When the generation is invoked
    Then the operation is rejected or marked non-authoritative
    And no governed receipt is emitted
