# CS2 Verification Experiment Corpus

This directory records reusable falsification cases for RFC-CS2-001 projections.

## Metamorphic properties

1. Reordering equivalent projection rows must preserve semantic identity.
2. Every generated row must retain the exact RFC-CS2-001 subject.
3. Generated authority must remain CONSTRUCT; mutation to DO is a failing case.
4. Divergent-subject fixtures must not manufacture canonical consumers.
5. Repeated generation must be byte-identical.

## Mutation corpus

- subject replacement
- authority escalation
- duplicate projection row
- missing work identifier
- consumer/projection permutation
- receipt digest mutation
- generated-file drift between first and second replay

These cases are experimental falsifiers, not standing claims.
