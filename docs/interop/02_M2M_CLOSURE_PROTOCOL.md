<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GENESIS MACHINE-TO-MACHINE CLOSURE PROTOCOL](#genesis-machine-to-machine-closure-protocol)
  - [Core Doctrine](#core-doctrine)
  - [Non-negotiable Boundaries](#non-negotiable-boundaries)
  - [Required Agent Loop](#required-agent-loop)
  - [Completion Standard](#completion-standard)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GENESIS MACHINE-TO-MACHINE CLOSURE PROTOCOL

The human has completed the architectural thinking.

Your job is not to reinterpret, brainstorm, or ask the human to decide between good code and bad code.

Your job is to convert the accepted architecture into repo evidence, implementation, tests, documentation, receipts, replay, and refusal.

## Core Doctrine

Genesis separates consequence from contact.

- **ggen** handles contact (adapters, files, APIs, external formats, CLI, packaging, projections, and validation bridges).
- **Genesis** handles consequence (pure math kernel, A = μ(O)).
- **AtomVM** gives custody.
- **WASM** gives portability.
- **Rust** gives physical discipline.
- **Truex** governs lifecycle.
- **wasm4pm/pictl** validate process projections.
- **External validators** consume projections.
- **Indexes** are derived views/access paths, not authority.

## Non-negotiable Boundaries

1. **Genesis must remain pure.** No outside-world dependencies inside Genesis.
2. **ggen owns the membrane.** All IO and format translation lives here.
3. **AtomVM/WASM are runtime bodies.** They are not "downstream"; they hold the kernel.
4. **Truex is lifecycle/promotion.** It is not the Genesis kernel.
5. **Pair2 is not compressed RDF.** It is `left byte + right byte` under a predicate-fixed `RelationPage`.
6. **Need9/Need257 means split.** No widening of hot payloads.
7. **No unsupported implementation claims.** Every claim must have file evidence.
8. **No “done” claim without tests or explicit refusal.**

## Required Agent Loop

For every task:

1. **RESEARCH**
   - inspect files, tests, manifests, docs, and build status.
2. **CLASSIFY**
   - Tag status as: IMPLEMENTED, PARTIAL, DOC_ONLY, TEST_ONLY, DEAD_CODE, MISSING, or AMBIGUOUS.
3. **PATCH**
   - make the smallest boundary-preserving change.
   - do not broaden scope or rename globally without a migration plan.
4. **VERIFY**
   - compile, run relevant tests, check docs/examples.
5. **RECEIPT**
   - list changed files, commands run, pass/fail results, and remaining refusals.
6. **REFUSE**
   - if evidence is missing, say MISSING.
   - if implementation is partial, say PARTIAL.
   - if tests do not pass, say NOT DONE.
   - never replace missing proof with narrative.

## Completion Standard

A task is complete only when:
- files changed are listed.
- tests/commands are listed.
- results are listed.
- unsupported claims are removed.
- remaining gaps are explicit.
- the Genesis/ggen/runtime/Truex boundary is preserved.
