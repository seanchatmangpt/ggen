# 198. CycloneDX and SPDX

> **Pattern 198 · Receipts and Standing**
>
> **Standing rule:** this pattern is `ALIVE` only when its consequence has been observed in a real consumer and bound to replayable evidence.

## Context

Every important state transition needs durable evidence linking input, policy, output, and verification. The narrow problem in this chapter is **198. CycloneDX and SPDX**. The chapter converts a broad design claim into a named artifact, an executable check, and a refusal condition.

A weak implementation can usually demonstrate that a file rendered once. That is not the target. The target is a production rule whose input is admitted, whose output has one owner, whose behavior is observed by a real consumer, and whose standing can be replayed from durable evidence. Every claim in this chapter must therefore terminate in a path, command, test, fixture, digest, or receipt.

## Problem

Without a stable pattern for **CycloneDX and SPDX**, locally reasonable decisions accumulate into a pack that renders but does not manufacture a substitutable product. The defect normally appears later—as graph contamination, hidden handler work, path collision, drift, false proof, or an unverifiable release claim—when repair is more expensive and evidence is weaker.

## Forces

- The source of truth must remain distinguishable from every generated projection.
- A successful render is weaker than an admitted, consumed, independently verified consequence.
- Composition creates hidden coupling unless identity, ownership, and output boundaries are explicit.
- Fast regeneration is useful only when unchanged inputs reproduce unchanged bytes and changed inputs expose drift.
- Every actuation must terminate in a durable receipt or an explicit typed refusal.

## Governing law

\[A=\mu(O^{\star})\quad\text{with standing carried by named evidence}\]

The equation is read operationally. Observation is not accepted merely because it exists. It must enter the admitted set \(O^\star\). Manufacturing \(\mu\) must preserve the distinctions required by the downstream customer. The resulting artifact has standing only when the acceptance evidence names the exact source law, generator, output, and verification result.

## Therefore

Establish **CycloneDX and SPDX** as a named part of the pack contract rather than leaving it as convention or tacit knowledge.

Do this at the narrowest layer that owns the invariant. Keep graph-domain construction reversible until admission is complete. Route filesystem or external effects through the engine’s declared write path. Require the consumer and verifier to observe the intended consequence independently of the authoring convenience that produced it.

## Configuration

1. Admit the source.
2. Apply the pattern at its owning layer.
3. Actuate only through the declared generator path.
4. Verify in a clean consumer.
5. Preserve the receipt and refusal evidence.

### Crate alignment

The pattern is grounded in these live ownership surfaces:

- `praxis-core`
- `ggen-engine`
- `ggen-graph`
- `ggen-marketplace`

A pack may depend on these surfaces, but it must not silently absorb their responsibilities.

## Reference implementation

The companion listing is stored at [`src/listings/198-198-cyclonedx-and-spdx.json`](../listings/198-198-cyclonedx-and-spdx.json). It is intentionally small enough to inspect while retaining the chapter's load-bearing boundary.

```json
{
  "schema": "https://example.org/ggen/receipt/v1",
  "chapter": 198,
  "subject": "198. CycloneDX and SPDX",
  "version": "v26.7.19",
  "inputs": {
    "ontology_sha256": "<record-at-run-time>",
    "template_sha256": "<record-at-run-time>"
  },
  "checks": [
    "shape-admission",
    "sync",
    "compile",
    "test",
    "sync-twice",
    "manifest-diff"
  ],
  "outcome": "must-be-recorded-not-assumed"
}
```

## Verification procedure

Run the listing as part of the chapter laboratory rather than copying it into an unrelated scratch file. A valid verification cycle is:

```text
admit source → sync → build consumer → run independent proof → sync again → compare bytes → emit receipt
```

The proof must be non-vacuous. For code generation, changing a generated signature, dropping an ontology individual, widening an optional value, changing a Japanese identifier, or removing a refusal branch must cause a relevant check to fail. For release assets, removing a workflow, target family, package manifest, or attestation input must appear as manifest drift.

Use the verification ladder in order:

```text
unit → integration → end-to-end → adversarial mutation → idempotency → receipt verification
```

A lower rung may establish `PARTIAL_ALIVE`; it cannot establish the crown claim of the higher rungs.

Use the verification ladder in order:

```text
unit → integration → end-to-end → adversarial mutation → idempotency → receipt verification
```

A lower rung may establish `PARTIAL_ALIVE`; it cannot establish the crown claim of the higher rungs.

## Resulting context

Inputs, graph state, templates, outputs, verification, and signatures form a replayable provenance chain.

The pattern also creates obligations. The new source law must remain inspectable. Generated outputs must retain one owner. Consumers must not acquire hidden setup. Receipts must be regenerated whenever an admitted input or verifier changes. Neighboring patterns can now rely on this consequence without reintroducing the resolved ambiguity.

## Failure modes

- **Inert truth:** the ontology is accurate but no real consumer reads any generated output.
- **Self-proof:** subject and expected value are derived through the same defective projection.
- **Hidden handler gap:** the pack emits catalogs while the consumer must write the real behavior.
- **Union leakage:** a shared RDF class causes one pack to select another pack's individuals.
- **Multiple writers:** two templates or packs claim the same path.
- **False green:** a missing compiler, SDK, runner, or signer is reported as success instead of an explicit bounded outcome.

## Falsifier

Tampering with a recorded input, output, transition, or signature does not invalidate verification.

Execute that falsifier deliberately. A pattern with no plausible way to fail is a slogan, not production law.

## Continuous TCPS case study

The production release carries SHA-256 manifests, build/test receipts, SBOMs, and in-toto provenance while refusing to call missing toolchains a pass.

The TCPS project is treated as a conformance oracle rather than a marketing example. The recorded prototype generated the twenty-four-module core and ran the original Japanese `試験.rs` unchanged, with 14 of 14 tests passing after five module/struct shadowing corrections were disclosed. The wider project record also captured 20 of 20 `receiptctl` tests, 130 of 130 engine library tests, and 11 of 11 framework-pack end-to-end tests. These are project-recorded results; the book source preserves them as case-study evidence and does not pretend they were independently rerun in this artifact environment. (A 2026-07-19 rerun in the host ggen repository is recorded in the book's `SOURCE_NOTES.md`.)

## Laboratory

Create a minimal mutation related to **198. CycloneDX and SPDX**. Record the expected refusal or proof failure before applying the mutation. Run the complete lifecycle, preserve the failing receipt, restore or improve the source law, and rerun until the consumer passes. The final commit should change the manufacturing definition, not hand-edit the generated product.

## Acceptance gate

- [ ] The output for `chapter-198` is created by the declared pack path.
- [ ] A clean consumer can mount the output without hand-editing generated files.
- [ ] At least one independent check fails when the generated subject is intentionally mutated.
- [ ] A second sync is byte-identical when admitted inputs are unchanged.
- [ ] The receipt records the exact outcome rather than assuming success.

## Connections

This pattern receives its context from **Receipts and Standing** and passes a narrower, better-admitted state to the patterns that follow it in `SUMMARY.md`. When a later pattern fails, trace backward through source identity, admission, projection, ownership, consumer observation, and receipt rather than patching the generated artifact.

## Standing statement

The pattern is complete only when every checked acceptance item resolves to a concrete repository artifact or command result. Missing execution is `UNKNOWN`; a missing admitted dependency is `BLOCKED`; an unreachable verifier caused by the build is `BUILD_BROKEN`; an intentionally absent capability is `UNSUPPORTED`. None of those states may be reported as `ALIVE`.

## Repository capability alignment

This section is generated from the repository capability ledger. It distinguishes live machinery from demonstrated pack witnesses and from target-state guidance.

### Owning capabilities

- `input/output digests`
- `receipt chains`
- `verification evidence`
- `standing ceilings`

### Current repository evidence

- `crates/praxis-core/src/receipt_epoch.rs`
- `crates/ggen-engine/tests/receipt_chain_e2e.rs`
- `packs/ggen-verify-pack`

### Pack witnesses

- `packs/ggen-verify-pack`
- `packs/level-five-book-pack`

### Bounded standing

`PARTIAL`

### Open gap

Several equivalence classes remain explicitly bounded; presence in a receipt is not proof of semantic equivalence.

### Required falsifier

Run the narrow evidence path named above, then execute the chapter's consumer or mutation test. A missing tool, skipped consumer, stale lock, changed generated byte, or unverifiable receipt lowers standing; it never counts as success.
