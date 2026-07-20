# 82. Pack Gates as Production Law

> **Outcome:** Construct and verify the SPARQL admission gate for **82. Pack Gates as Production Law**.

This is chapter **82 of 336**. Its output becomes an input to the later certification laboratories.

## Production problem

A syntactically valid graph is not necessarily an admissible production input. The narrow problem in this chapter is **82. Pack Gates as Production Law**. The chapter converts a broad design claim into a named artifact, an executable check, and a refusal condition.

A weak implementation can usually demonstrate that a file rendered once. That is not the target. The target is a production rule whose input is admitted, whose output has one owner, whose behavior is observed by a real consumer, and whose standing can be replayed from durable evidence. Every claim in this chapter must therefore terminate in a path, command, test, fixture, digest, or receipt.

## Governing law

\[A=\mu(O^{\star})\quad\text{with standing carried by named evidence}\]

The equation is read operationally. Observation is not accepted merely because it exists. It must enter the admitted set \(O^\star\). Manufacturing \(\mu\) must preserve the distinctions required by the downstream customer. The resulting artifact has standing only when the acceptance evidence names the exact source law, generator, output, and verification result.

## Construction sequence

1. **Write positive constraints.** Identify the authoritative source and the exact downstream observable that must survive generation.
2. **Write negative fixtures.** Encode the rule in the appropriate layer: ontology, gate, query, template, generated type, consumer, or engine.
3. **Make generation fail closed.** Execute a check that can produce a typed failure and preserve that result in the evidence chain.

The sequence deliberately separates semantic work from filesystem actuation. RDF identities describe the domain. SPARQL admission gates (`gates/*.rq`) decide whether the input may proceed. SPARQL projections select the construction facts. Tera renders a deterministic projection. The target compiler and test runner then judge the produced artifact from the consumer's perspective.

## Reference implementation

The companion listing is stored at [`src/listings/082-82-pack-shapes-as-production-law.ttl`](../listings/082-82-pack-shapes-as-production-law.ttl). It is intentionally small enough to inspect while retaining the chapter's load-bearing boundary. The listing keeps its historical `.ttl` filename in this book's listing tree; in a shipped pack this file lives at `gates/010_admission.rq`.

```sparql
# MESSAGE: 82. Pack Gates as Production Law: malformed data must stop before rendering
# (translated from the retired SHACL shape: sh:minCount 1 / sh:maxCount 1 /
#  sh:datatype xsd:string on pack:生成単位 — any returned row is a violation)
PREFIX pack: <https://example.org/ggen/pack#>
PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>
SELECT ?s ?violation WHERE {
  {
    ?s a pack:生成単位 .
    VALUES ?required { pack:出力経路 pack:根拠 }
    FILTER NOT EXISTS { ?s ?required ?any }
    BIND(?required AS ?violation)
  } UNION {
    ?s a pack:生成単位 ; pack:出力経路 ?v1 , ?v2 .
    FILTER(?v1 != ?v2)
    BIND(pack:出力経路 AS ?violation)
  } UNION {
    ?s a pack:生成単位 ; pack:出力経路 ?v .
    FILTER(DATATYPE(?v) != xsd:string)
    BIND(pack:出力経路 AS ?violation)
  }
}
ORDER BY ?s ?violation
```

### How the current admission mechanism works

Since the engine's SPARQL-gate migration, a pack no longer ships SHACL. Admission law is a `gates/` directory of `.rq` files next to `ontology.ttl`, evaluated in sorted filename order (hence the `010_`, `020_`, `030_` numbering convention) against the post-materialization union graph. Each gate file is an optional block of leading `# MESSAGE:` comment lines (the operator-facing violation text) followed by exactly one ASK or SELECT query: ASK returning `true` is a violation, and any SELECT row is a violation; a CONSTRUCT or DESCRIBE query is not a gate and refuses with `[FM-PACK-012]`. A missing `gates/` directory simply means no gates, but an unreadable directory or file is a typed `[FM-PACK-012]` refusal (fail closed), a gate violation refuses the sync with `[FM-PACK-013]`, and a legacy `shapes.ttl` still present in a pack is itself a loud `[FM-PACK-012]` refusal — never silently ignored. Project-level `[law].gates` mirrors this with `[FM-LAW-012]`/`[FM-LAW-013]`, and declaring the retired `[law].shapes` key refuses with a migration message. Every gate file is hashed into the receipt closure. Because gates are plain SPARQL evaluated through `GraphEngine::query`, the identical gate refuses the identical facts under both the GraphLaw and the Oxigraph engine — proven by `crates/ggen-engine/tests/reasoner_independence_e2e.rs`, which syncs the same pack byte-identically under both backends and receives the same `[FM-PACK-013]` on the same violating fixture. SHACL could not make that promise: `validate_shacl` on the Oxigraph backend is a typed `[FM-LAW-002]` refusal.


## Verification procedure

Run the listing as part of the chapter laboratory rather than copying it into an unrelated scratch file. A valid verification cycle is:

```text
admit source → sync → build consumer → run independent proof → sync again → compare bytes → emit receipt
```

The proof must be non-vacuous. For code generation, changing a generated signature, dropping an ontology individual, widening an optional value, changing a Japanese identifier, or removing a refusal branch must cause a relevant check to fail. For release assets, removing a workflow, target family, package manifest, or attestation input must appear as manifest drift.

## Continuous TCPS case study

TCPS gates refuse missing source text, duplicate output ownership, invalid paths, and incomplete release records before generation.

The TCPS project is treated as a conformance oracle rather than a marketing example. The recorded prototype generated the twenty-four-module core and ran the original Japanese `試験.rs` unchanged, with 14 of 14 tests passing after five module/struct shadowing corrections were disclosed. The wider project record also captured 20 of 20 `receiptctl` tests, 130 of 130 engine library tests, and 11 of 11 framework-pack end-to-end tests. These are project-recorded results; the book source preserves them as case-study evidence and does not pretend they were independently rerun in this artifact environment. (A 2026-07-19 rerun in the host ggen repository is recorded in the book's `SOURCE_NOTES.md`.)

## Failure modes

- **Inert truth:** the ontology is accurate but no real consumer reads any generated output.
- **Self-proof:** subject and expected value are derived through the same defective projection.
- **Hidden handler gap:** the pack emits catalogs while the consumer must write the real behavior.
- **Union leakage:** a shared RDF class causes one pack to select another pack's individuals.
- **Multiple writers:** two templates or packs claim the same path.
- **False green:** a missing compiler, SDK, runner, or signer is reported as success instead of an explicit bounded outcome.

## Laboratory

Create a minimal mutation related to **82. Pack Gates as Production Law**. Record the expected refusal or proof failure before applying the mutation. Run the complete lifecycle, preserve the failing receipt, restore or improve the source law, and rerun until the consumer passes. The final commit should change the manufacturing definition, not hand-edit the generated product.

## Acceptance gate

- [ ] The output for `chapter-082` is created by the declared pack path.
- [ ] A clean consumer can mount the output without hand-editing generated files.
- [ ] At least one independent check fails when the generated subject is intentionally mutated.
- [ ] A second sync is byte-identical when admitted inputs are unchanged.
- [ ] The receipt records the exact outcome rather than assuming success.

## Standing statement

The chapter is complete only when each checked box resolves to a concrete artifact in the repository. Prose comprehension is necessary but insufficient; the reader must be able to reproduce the transition and observe both its success and its refusal behavior.
