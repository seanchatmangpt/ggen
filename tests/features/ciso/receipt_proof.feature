# language: en
@receipt @proof @ciso @tier1 @existential
Feature: Receipt proof and auditability

  As a CISO operating an enterprise code generation system
  I want cryptographic receipts to prove build provenance
  So that every artifact can be traced to its source and validated

  Background:
    Given the workspace is initialized with Ed25519 keys
    And the receipt directory exists at ".ggen/receipts"

  @tier1 @receipt_emitted
  Scenario: Sync emits a composition receipt
    Given a successful sync
    When artifact emission completes
    Then a receipt is written
    And the receipt contains the pack graph
    And the receipt contains versions and digests
    And the receipt contains queries, templates, validators, and artifact hashes

  @tier1 @receipt_verify_valid
  Scenario: Receipt verify succeeds for untampered receipt
    Given a valid receipt file at ".ggen/receipts/latest.json"
    When the operator runs "ggen receipt verify .ggen/receipts/latest.json"
    Then verification succeeds
    And the result reports valid signature and hash

  @tier1 @receipt_verify_tampered
  Scenario: Receipt verify fails for tampered receipt
    Given a receipt file ".ggen/receipts/tampered.json" whose contents were modified after signing
    When the operator runs "ggen receipt verify .ggen/receipts/tampered.json"
    Then verification fails

  @tier1 @receipt_info_metadata
  Scenario: Receipt info exposes composition metadata
    Given a valid receipt file ".ggen/receipts/latest.json"
    When the operator runs "ggen receipt info .ggen/receipts/latest.json"
    Then the output includes contributing packs
    And the output includes runtime, profile, and registry source

  @tier1 @chain_verify_valid
  Scenario: Chain verify succeeds across intact receipt lineage
    Given a series of receipts linked by generation lineage
    When the operator runs "ggen receipt chain-verify .ggen/receipts/latest.json"
    Then chain verification succeeds

  @tier1 @chain_verify_broken
  Scenario: Chain verify fails when lineage is broken
    Given a missing or altered intermediate receipt in the chain
    When the operator runs "ggen receipt chain-verify .ggen/receipts/latest.json"
    Then verification fails
