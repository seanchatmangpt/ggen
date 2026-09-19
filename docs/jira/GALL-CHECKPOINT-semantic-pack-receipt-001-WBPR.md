# WBPR — GALL-001 Semantic Pack Receipt Replay Seal

**Working-backwards target. This document describes the release state to be achieved; it is not evidence that the checkpoint is complete today.**

## Headline

**ggen makes semantic-pack manufacture replayable from evidence, not memory.**

## Subheadline

For the first time in the GALL release chain, a generated artifact can be traced back to an exact admitted pack, canonical graph, dependency closure, engine/toolchain identity, consequence set, and portable receipt — then regenerated from clean state and independently shown to be the same semantic manufacture.

## Announcement

At completion of GALL-001, `ggen` closes the first release-critical transition in Semantic A2A:

`semantic pack -> admission -> deterministic manufacture -> portable receipt -> clean replay -> standing`

The release does not ask downstream systems to trust that generation “worked.” It gives them an exact subject and a replayable receipt whose claims can be reconstructed without using the first generated output as its own oracle.

## Customer problem

Code generation is easy to demonstrate and hard to trust at system scale.

A generator can emit correct-looking files while leaving unanswered questions:

- Which exact semantic pack produced these bytes?
- Which dependency closure participated?
- Which graph revision was admitted?
- Which engine/toolchain manufactured the output?
- Did a later hand edit silently change the artifact?
- Can a second clean execution recreate the same consequence set?
- Is the receipt evidence, or just another generated claim?

Without those answers, downstream systems must re-reason about provenance or trust mutable local state.

## Product

GALL-001 turns the existing RFC-GPACK portable receipt into an executable replay contract.

A successful receipt binds:

- exact repository revision;
- exact pack/version/PackDigest;
- canonical graph digest;
- declared dependency closure and dependency digests;
- engine/toolchain identity;
- gates attempted and typed refusals;
- generated consequences and SHA-256 identities;
- replay command and result;
- exact standing.

The generated files remain projections. The receipt reports standing; it does not create authority.

## What changes for a consumer

Before GALL-001:

`artifact -> "the generator says this came from pack X"`

After GALL-001:

`artifact -> receipt -> exact semantic subject -> clean replay -> same artifact identity`

A consumer no longer needs the producer's previous process, cache, or workspace to decide whether the artifact belongs to the claimed semantic subject.

## Core invariant

`ReplayPASS => ExactSubjectMatch && IndependentInputIdentity && IndependentOutputIdentity && CleanRegeneration`

And:

`GeneratedCode != SemanticTruth`

`Receipt != Authority`

## Release proof

The checkpoint is releasable only when one exact PR head proves all of the following:

1. a real admitted pack executes from clean state;
2. PackDigest is independently recomputed;
3. canonical graph and declared dependency closure identities are independently recomputed;
4. generated consequence identities are independently recomputed;
5. a second clean execution yields the same semantic consequence set;
6. at least one pack/graph/dependency/output mutation invalidates replay standing;
7. a refused pack cannot manufacture replay PASS;
8. the portable envelope records replay PASS only after observed execution.

## Chicago relation

GALL-001 primarily supplies exact-identity, executable-world, receipt-binding, and replay evidence to the later Chicago crown.

It does **not** prove authority, DO, independent postcondition observation, or fresh-consumer reconstruction.

## Non-claims

This release does not claim:

- production deployment;
- SA2A authority;
- BRCE/CommandBus actuation;
- process conformance analysis inside ggen;
- cross-repository ALIVE standing;
- publication merely because a PR exists.

## Release receipt

The release is complete only when the exact-head checkpoint receipt can answer mechanically:

- What exact source executed?
- Which pack/dependencies were admitted?
- Which graph was manufactured?
- Which files changed?
- Which digests bind those files?
- Which falsifier was attempted?
- Did clean replay reproduce the same subject?
- What evidence supports the standing?

## Working-backwards definition of done

A downstream system can receive a GALL-001 receipt and treat semantic-pack manufacture as a **known, replayable machine transformation** rather than a generator assertion.

That is the release boundary:

`UNKNOWN manufacture -> receipted deterministic manufacture -> reusable machine fact`
