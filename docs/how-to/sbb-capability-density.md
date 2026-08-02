# Measure and distribute SBB capability density

This walkthrough creates an evidence-bound Solution Building Block capability manifest, evaluates its commit-equivalent density, emits receipts, and replays the result.

## 1. Choose the architecture contract and distribution boundary

Declare the SBB identity and a positive density threshold. The service-provider profile uses `1000`:

```json
{
  "schema": "ggen.sbb.capability-manifest.v1",
  "sbb": {
    "id": "regulated-service-foundation",
    "version": "1.0.0",
    "architecture_contract": "urn:example:architecture-contract:regulated-service-foundation",
    "minimum_commit_equivalent_units": 1000
  },
  "repository": {
    "root": "."
  }
}
```

The target is not evidence. The evaluator remains below target until 1,000 unique commits have complete observed chains.

## 2. Declare every distribution axis

Every axis is non-empty and duplicate-free:

```json
{
  "distribution": {
    "ontology_modules": ["urn:example:ontology:service", "urn:example:ontology:audit"],
    "textual_forms": ["rust", "openapi", "markdown", "mermaid", "policy-prose"],
    "audiences": ["developer", "operator", "auditor"],
    "languages": ["en"],
    "jurisdictions": ["global", "us-ca"],
    "organization_profiles": ["provider-default", "customer-overlay"],
    "runtimes": ["native", "wasm"]
  }
}
```

The evaluator computes the Cartesian product with checked arithmetic. Projected instances are distribution effects, not new canonical capabilities.

## 3. Bind one capability delta to one Git commit

Each counted delta needs:

- one unique commit SHA;
- one capability IRI;
- ontology and textual-form membership;
- the complete ten-stage manufacturing chain;
- a positive witness;
- a negative fixture;
- an adversarial falsifier;
- an executable verifier.

Each evidence object uses a safe repository-relative path and the BLAKE3 digest of the bytes stored at the claimed commit:

```json
{
  "id": "service-receipt-replay",
  "commit": "<git-commit-sha>",
  "capability_iri": "urn:example:capability:service-receipt-replay",
  "family": "receipt",
  "summary": "Replays service receipts and refuses digest divergence.",
  "ontology_modules": ["urn:example:ontology:audit"],
  "textual_forms": ["rust", "markdown"],
  "chain": {
    "ontology": {"locator": "ontology/service-receipt.ttl", "digest": "blake3:<64-hex>"},
    "shacl": {"locator": "ontology/service-receipt.shacl.ttl", "digest": "blake3:<64-hex>"},
    "sparql": {"locator": "queries/service-receipt.rq", "digest": "blake3:<64-hex>"},
    "typestate": {"locator": "src/service_receipt.rs", "digest": "blake3:<64-hex>"},
    "template": {"locator": "templates/service-receipt.md.tera", "digest": "blake3:<64-hex>"},
    "artifact": {"locator": "fixtures/service-receipt.json", "digest": "blake3:<64-hex>"},
    "runtime_surface": {"locator": "src/cmds/service_receipt.rs", "digest": "blake3:<64-hex>"},
    "walkthrough": {"locator": "docs/how-to/service-receipt.md", "digest": "blake3:<64-hex>"},
    "receipt": {"locator": "fixtures/service-receipt-result.json", "digest": "blake3:<64-hex>"},
    "replay": {"locator": "fixtures/service-receipt-replay.json", "digest": "blake3:<64-hex>"}
  },
  "positive_witness": {"locator": "tests/service_receipt_positive.rs", "digest": "blake3:<64-hex>"},
  "negative_fixture": {"locator": "tests/fixtures/service_receipt_diverged.json", "digest": "blake3:<64-hex>"},
  "adversarial_falsifier": {"locator": "tests/service_receipt_adversarial.rs", "digest": "blake3:<64-hex>"},
  "verifier": {"locator": "tests/service_receipt_verifier.rs", "digest": "blake3:<64-hex>"}
}
```

Do not calculate the digest from uncommitted working-tree bytes. The runtime verifies `git show <commit>:<locator>` and hashes those historical bytes.

## 4. Inspect without mutation

```text
ggen sbb inspect path/to/sbb-capability-manifest.json
```

The report names every observed delta and every refusal. The command performs no writes and no network access.

## 5. Validate the density threshold

```text
ggen sbb validate path/to/sbb-capability-manifest.json
```

A passing result means the manifest is eligible for external admission. It does not mean the evaluator has certified itself as `ALIVE`.

Duplicate commit claims cannot inflate the result. If two deltas claim the same commit, both claims are refused and contribute zero units.

## 6. Examine the distribution multiplier

```text
ggen sbb distribution path/to/sbb-capability-manifest.json
```

The output distinguishes:

- `canonical_maintenance_units`: unique observed capability commits maintained once;
- `distribution_contexts`: lawful combinations of the declared axes;
- `delivered_capability_instances`: their product.

This is the service-provider transformation: maintain one admitted lineage and distribute its effects without recreating a bespoke engineering history for every customer.

## 7. Emit the report and chained receipts

```text
ggen sbb receipt path/to/sbb-capability-manifest.json .ggen/sbb/regulated-service-foundation
```

The bounded write produces:

```text
.ggen/sbb/regulated-service-foundation/
├── density-intent.json
├── density-report.json
└── density-result.json
```

No cloud, deployment, registry, or other external actuation occurs.

## 8. Replay

```text
ggen sbb replay path/to/sbb-capability-manifest.json .ggen/sbb/regulated-service-foundation
```

The valid terminal result is:

```text
REPLAY_MATCH
```

A changed manifest, changed Git-object evidence, invalid receipt digest, or broken intent/result link returns `REPLAY_DIVERGED`.

## 9. External admission

An independent verifier must execute the exact-head repository checks, inspect the aggregate receipt, and decide whether the result has `ALIVE` standing. The SBB evaluator’s maximum self-reported standing remains `PARTIAL_ALIVE`.
