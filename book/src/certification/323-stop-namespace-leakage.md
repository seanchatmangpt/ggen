# 323. Laboratory Three: Stop Namespace Leakage

> **Outcome:** Construct and verify the certification bundle for **323. Laboratory Three: Stop Namespace Leakage**.

This is chapter **323 of 336**. Its output becomes an input to the later certification laboratories.

## Production problem

Certification is adversarial construction: each laboratory introduces a failure class and requires a durable countermeasure. The narrow problem in this chapter is **323. Laboratory Three: Stop Namespace Leakage**. Shared classes are safe only when local individuals remain scoped; union graphs make implicit assumptions visible.

A weak implementation can usually demonstrate that a file rendered once. That is not the target. The target is a production rule whose input is admitted, whose output has one owner, whose behavior is observed by a real consumer, and whose standing can be replayed from durable evidence. Every claim in this chapter must therefore terminate in a path, command, test, fixture, digest, or receipt.

## Governing law

\[A=\mu(O^{\star})\quad\text{with standing carried by named evidence}\]

The equation is read operationally. Observation is not accepted merely because it exists. It must enter the admitted set \(O^\star\). Manufacturing \(\mu\) must preserve the distinctions required by the downstream customer. The resulting artifact has standing only when the acceptance evidence names the exact source law, generator, output, and verification result.

## Construction sequence

1. **Inject a defect.** Identify the authoritative source and the exact downstream observable that must survive generation.
2. **Observe refusal.** Encode the rule in the appropriate layer: ontology, gate, query, template, generated type, consumer, or engine.
3. **Prove the countermeasure.** Execute a check that can produce a typed failure and preserve that result in the evidence chain.

The sequence deliberately separates semantic work from filesystem actuation. RDF identities describe the domain. SPARQL admission gates (`gates/*.rq`) decide whether the input may proceed. SPARQL projections select the construction facts. Tera renders a deterministic projection. The target compiler and test runner then judge the produced artifact from the consumer's perspective.

## Reference implementation

The companion listing is stored at [`src/listings/323-323-laboratory-three-stop-namespace-leakage.sh`](../listings/323-323-laboratory-three-stop-namespace-leakage.sh). It is intentionally small enough to inspect while retaining the chapter's load-bearing boundary.

```bash
#!/usr/bin/env bash
set -euo pipefail

subject="323. Laboratory Three: Stop Namespace Leakage"
before="$(mktemp -d)"
after="$(mktemp -d)"

ggen sync run
cp -R src "$before/"
cargo test --all-targets
ggen sync run
cp -R src "$after/"

diff -ru "$before/src" "$after/src"
printf 'verified: %s\n' "$subject"
```

## Verification procedure

Run the listing as part of the chapter laboratory rather than copying it into an unrelated scratch file. A valid verification cycle is:

```text
admit source → sync → build consumer → run independent proof → sync again → compare bytes → emit receipt
```

The proof must be non-vacuous. For code generation, changing a generated signature, dropping an ontology individual, widening an optional value, changing a Japanese identifier, or removing a refusal branch must cause a relevant check to fail. For release assets, removing a workflow, target family, package manifest, or attestation input must appear as manifest drift.

## Continuous TCPS case study

The final audit reproduces the exact classes of abnormalities found during TCPS and multi-pack integration before granting Level Five standing.

The TCPS project is treated as a conformance oracle rather than a marketing example. The recorded prototype generated the twenty-four-module core and ran the original Japanese `試験.rs` unchanged, with 14 of 14 tests passing after five module/struct shadowing corrections were disclosed. The wider project record also captured 20 of 20 `receiptctl` tests, 130 of 130 engine library tests, and 11 of 11 framework-pack end-to-end tests. These are project-recorded results; the book source preserves them as case-study evidence and does not pretend they were independently rerun in this artifact environment. (A 2026-07-19 rerun in the host ggen repository is recorded in the book's `SOURCE_NOTES.md`.)

## Failure modes

- **Inert truth:** the ontology is accurate but no real consumer reads any generated output.
- **Self-proof:** subject and expected value are derived through the same defective projection.
- **Hidden handler gap:** the pack emits catalogs while the consumer must write the real behavior.
- **Union leakage:** a shared RDF class causes one pack to select another pack's individuals.
- **Multiple writers:** two templates or packs claim the same path.
- **False green:** a missing compiler, SDK, runner, or signer is reported as success instead of an explicit bounded outcome.

## Laboratory

Create a minimal mutation related to **323. Laboratory Three: Stop Namespace Leakage**. Record the expected refusal or proof failure before applying the mutation. Run the complete lifecycle, preserve the failing receipt, restore or improve the source law, and rerun until the consumer passes. The final commit should change the manufacturing definition, not hand-edit the generated product.

## Acceptance gate

- [ ] The output for `chapter-323` is created by the declared pack path.
- [ ] The result is compared with the TCPS v26.7.19 reference or an explicitly named derivative oracle.
- [ ] At least one independent check fails when the generated subject is intentionally mutated.
- [ ] A second sync is byte-identical when admitted inputs are unchanged.
- [ ] The laboratory produces a certification artifact suitable for inclusion in the final Level Five bundle.

## Standing statement

The chapter is complete only when each checked box resolves to a concrete artifact in the repository. Prose comprehension is necessary but insufficient; the reader must be able to reproduce the transition and observe both its success and its refusal behavior.
