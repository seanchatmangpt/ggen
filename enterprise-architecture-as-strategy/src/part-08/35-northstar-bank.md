# 35. Synthetic Case Study: Northstar Federated Bank

Northstar is a fictional multinational bank with retail, commercial, wealth, and payments businesses. Its operating model requires integration of risk and identity while preserving product autonomy.

## Strategic Problem

Customer identity, entitlement, product, and risk facts are inconsistent across lines of business. Regulatory obligations vary by jurisdiction. Centralization attempts have failed because they demanded one implementation rather than one semantic contract.

## Target Architecture

Northstar adopts a federated model:

- canonical identity and entitlement ontology;
- local product ontologies;
- shared policy and provenance vocabulary;
- distribution packs for consumer integration;
- bounded regional overlays;
- common evidence and receipt model.

## Architecture Products

The central architecture group publishes a Customer and Entitlement Distribution Pack. It exposes meanings, constraints, compatibility, conformance tests, and extension points. It does not force one database or language.

Consumers generate:

- APIs;
- local schemas;
- IAM policies;
- data-handling rules;
- audit mappings;
- monitoring;
- migration adapters.

## Transition

The migration uses coexistence. Legacy identities remain active while correlation confidence is measured. High-risk actions require stronger identity standing. Retirement occurs only after consumer evidence shows that no critical workflow depends on an uncorrelated identity.

## Formal and Runtime Assurance

mfact proves properties of entitlement composition within the bounded model. Correspondence tests connect generated policy implementations to the formal rules. wasm4pm observes customer onboarding and privileged-access workflows.

## Failure Test

A regional business imports an internal producer ontology directly instead of the distribution contract. The architecture doctor refuses promotion because hidden authority and private dependencies crossed the boundary.

## Success Measure

Northstar succeeds when risk and identity are integrated semantically while product teams retain implementation choice. The measure is reduced manual policy restatement and improved cross-product evidence, not a single universal platform.
