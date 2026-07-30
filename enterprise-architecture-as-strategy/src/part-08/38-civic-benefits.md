# 38. Synthetic Case Study: Civic Benefits Administration

Civic Benefits Administration is a fictional public agency delivering income, housing, disability, food, and emergency assistance through federal, state, local, and nonprofit partners.

## Strategic Problem

Programs have different statutes, eligibility rules, evidence standards, application channels, and review procedures. Residents repeatedly submit the same information. Agencies cannot safely share meaning because authority and purpose differ.

## Target Operating Model

The architecture separates:

- resident facts;
- program-specific eligibility;
- evidence provenance;
- consent and legal authority;
- decision explanation;
- appeal rights;
- case workflow;
- payment execution.

Shared semantics do not imply shared authorization.

## Generative Consequences

Program law and policy ontologies generate:

- application schemas;
- plain-language explanations;
- eligibility prechecks;
- staff decision aids;
- evidence checklists;
- audit trails;
- appeal packets;
- accessibility metadata;
- multilingual content;
- process-conformance rules.

## Human Authority

The system may derive candidate eligibility and missing evidence. It does not erase due process or delegate legally reserved decisions without explicit authority.

## Change Management

When a statute changes, semantic diff identifies affected programs, forms, rules, documentation, tests, training, and active cases. A transition pack supports coexistence before and after the effective date.

## Refusal

A decision cannot be promoted when explanation lineage is missing or when a generated rule exceeds admitted legal authority.

## Measure

Success is reduced resident restatement, faster lawful decisions, clearer appeals, and stronger evidence. The case demonstrates that Combinatorial Maximalism can improve service while preserving jurisdiction and rights.
