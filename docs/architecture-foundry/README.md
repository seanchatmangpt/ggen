# ggen Enterprise Architecture Foundry

**Status:** governing plan for implementation and agent execution.

## Purpose

The ggen Enterprise Architecture Foundry converts proven software systems into executable architecture knowledge and uses that knowledge to manufacture new, independently verified solution architectures.

This is not a conventional archive, rewrite, modernization program, or code migration. It is a two-direction manufacturing system:

1. **Architecture Corpus Formation** — observe an existing system, recover its behavioral and architectural contract, admit the evidence, and form reusable architecture primitives.
2. **Solution Architecture Manufacture** — combine admitted primitives with a new organization's requirements and constraints to manufacture repositories, controls, evidence, and operating architecture.

## Repository topology

| Repository | Governing role |
|---|---|
| `seanchatmangpt/ggen` | Repository Manufacturing Kernel: observation admission, ontology validation, planning, composition, projection, verification, receipts, replay, standing, and actuation control. |
| `seanchatmangpt/ggen-legacy` | Enterprise Architecture Foundry Corpus: historical implementations, admitted capability records, generalized architecture primitives, packs, reference witnesses, migration evidence, negative controls, and reusable solution architectures. |
| Manufactured customer repositories | Admitted solution instances generated from customer observations plus selected foundry primitives. |

`ggen-legacy` is not a dead repository. In this program, *legacy* means a system with sufficient accumulated evidence to be mined for reusable architectural knowledge.

## Product vocabulary

- **Category:** Enterprise Architecture Reconstitution and Manufacture
- **Platform:** ggen Enterprise Architecture Foundry
- **Method:** Evidence-Driven Repository Manufacture
- **Extraction phase:** Architecture Corpus Formation
- **Generalization phase:** Enterprise Architecture Mining
- **Production phase:** Solution Architecture Manufacture
- **Evidence package:** Architecture Standing Portfolio
- **Completion event:** Solution Admission
- **Retirement event:** Sunset Admission
- **Ongoing service:** Architecture Foundry Operations

## Core theorem

A solution architecture has standing only when its authority, implementation, witnesses, falsifiers, receipts, and replay agree at one exact source head.

The foundry therefore operates under these invariants:

- zero unreceipted actuation;
- no self-certification;
- no unknown capability at final admission;
- no unknown legacy disposition at final admission;
- no status promotion from agent completion alone;
- no hand-edited generated authority;
- no retirement without executable equivalence or explicit refusal and migration evidence;
- no clean-room claim without a fresh exact-head execution;
- no solution admission without deterministic replay.

## Bootstrap case

The `ggen-legacy` to `ggen` v26.8.1 rebuild is the first complete reference implementation of the foundry method. It must prove that:

1. a large historical repository can be observed exhaustively;
2. its capabilities can be assigned evidence-supported dispositions;
3. the manufacturing kernel can be separated from historical solution material;
4. extracted solution material can remain productive in `ggen-legacy`;
5. reusable architecture primitives and packs can be derived from that material;
6. a clean repository can be manufactured from admitted authority;
7. independent verifiers can assign standing without consuming self-authored status;
8. replay can prove semantic and generated stability;
9. the same foundry corpus can support future Fortune-scale solution architectures.

## Execution entrypoints

Agents must read, in order:

1. repository `AGENTS.md` and all applicable local constitutions;
2. [`REPOSITORY_ROLES.md`](./REPOSITORY_ROLES.md);
3. [`GGEN_LEGACY_MIGRATION_PLAN.md`](./GGEN_LEGACY_MIGRATION_PLAN.md);
4. [`AGENT_EXECUTION_CONTRACT.md`](./AGENT_EXECUTION_CONTRACT.md);
5. [`work-program.yaml`](./work-program.yaml).

Commercial framing is defined in [`SERVICE_CATALOG.md`](./SERVICE_CATALOG.md).

## Final state

The program is complete only when:

```text
kernel_unknowns=0
corpus_unknowns=0
unresolved_dispositions=0
unassigned_verifiers=0
missing_equivalence_cases=0
equivalence_failures=0
replay_differences=0
ggen_release_admitted=true
ggen_legacy_corpus_admitted=true
cross_repository_receipts_valid=true
standing=ALIVE
```

Anything less is an intermediate standing, not completion.
