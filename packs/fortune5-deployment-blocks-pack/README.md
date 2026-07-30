# fortune5-deployment-blocks-pack

This pack is the ontology authority for `ggen bblock <verb>`. It declares the globally available Fortune 5 deployment surface for AWS, Azure, and Google Cloud Platform, including the compatibility alias `gpc` for `gcp`.

## Command surface

```bash
ggen bblock providers
ggen bblock list
ggen bblock inspect fortune5-complete aws
ggen bblock group fortune5-platform azure
ggen bblock plan fortune5-complete gpc
ggen bblock enable fortune5-complete aws
ggen bblock validate
```

The Rust command is a generic catalog compiler. It contains no AWS, Azure, or GCP service-selection branches. Provider aliases, group dependencies, pack identities, and directory placement are retained projections of `ontology.ttl` and independently checked for equivalence in CI.

## Group-of-packs law

Each `bb:BlockGroup` declares:

- stable identity, title, and description;
- one repository-relative output directory;
- zero or more dependencies on other groups;
- common constitutional packs;
- AWS, Azure, and GCP package projections.

Resolution is transitive, dependency-first, deterministic, and duplicate-free. Unknown providers, unknown groups, malformed package identities, unsafe paths, missing provider projections, and cycles are typed refusals.

## Canonical groups

Atomic groups cover:

- global network and DNS;
- workload identity;
- encryption and KMS;
- comprehensive observability;
- supervised container runtime;
- serverless compute;
- transactional data;
- object and evidence storage;
- event and message fabric;
- artifact and supply-chain registry;
- policy and governance;
- resilience and disaster recovery;
- global edge delivery;
- evidence and receipt ledger.

Composite groups provide:

- `fortune5-foundation`;
- `fortune5-platform`;
- `fortune5-control-plane`;
- `fortune5-complete`.

## Directory law

`plan` and `enable` use a fixed repository-local control surface:

```text
.ggen/bblocks/
├── groups/<group>.json
├── plans/<provider>/<group>.json
└── receipts/<provider>/
    ├── <group>-plan-intent.json
    ├── <group>-plan-result.json
    ├── <group>-enable-intent.json
    └── <group>-enable-result.json
```

Resolved infrastructure directories are created only by `enable`, using the ontology-declared `outputDirectory` values under `infrastructure/`. Pack identities are added to `.ggen/packs.lock` with a catalog-bound BLAKE3 integrity value.

## Actuation boundary

`bblock` performs local project construction only:

```text
ontology-derived catalog
→ dependency closure
→ deterministic plan
→ intent receipt
→ local directories and packs.lock
→ result receipt
```

It never invokes a cloud API, Terraform, Pulumi, Kubernetes, a shell deployment command, or a network client. Actual infrastructure actuation remains downstream of admitted packs and BRCE.
