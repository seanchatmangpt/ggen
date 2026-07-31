# ggen v26.8.1 Planning Corpus

This directory is the planning projection of the unified v26.8.1 ontology. It intentionally uses a **planner portfolio**, because no single standardized planner dialect faithfully supports every required semantic dimension.

## Dialects

- `domains/ggen-v2681-core.pddl` — PDDL 3.1-style classical planning with typing, quantified and disjunctive conditions, conditional effects, derived predicates, numeric fluents, action costs, preferences, and trajectory constraints.
- `domains/ggen-v2681-temporal.pddl` — temporal-numeric planning with durative actions, bounded durations, resources, invariants, continuous effects, timed obligations, and concurrency safety.
- `domains/ggen-v2681-probabilistic.ppddl` — PPDDL uncertainty model for flaky verifiers, recovery, risk, and expected reward.
- `domains/ggen-v2681-hierarchical.hddl` — HDDL task decomposition for corpus closure, subsystem admission, crown promotion, and legacy sunset.

## Problem coverage

Ten deterministic problem families cover all 100 numbered v26.8.1 research subsystems. A probabilistic recovery problem covers verifier uncertainty. Every problem is a projection; `ontology/v26.8.1/ontology.ttl` remains semantic authority.

## Hard laws

1. No actuation without BRCE admission.
2. No subsystem admission without positive witness, negative falsifier, replay, verifier, and receipt.
3. No release promotion while unresolved coverage exists.
4. No legacy sunset while any capability has information loss or unknown disposition.
5. Planner success is evidence only after the plan is independently validated and receipt-bound.

## Verification

Run:

```bash
just --justfile planning/v26.8.1/justfile verify
```

The structural verifier checks balanced forms, domain/problem identity, required dialect features, ten-family coverage, PPDDL/HDDL presence, ontology bindings, and SHA-256 evidence output. The repository's real `bcinr-pddl` tests are then executed as the parser/planner boundary.

The corpus is ambitious, but it is not described as “the most advanced of all time” until comparative planner execution evidence supports that statement.
