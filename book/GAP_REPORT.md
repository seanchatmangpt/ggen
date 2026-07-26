# Book Capability Gap Audit

This report is an observed current-head census. A green broad workspace test does not erase the gaps listed here.

## Claim graph

```text
GAP DECLARED_PARTIAL packs/cargo-cicd-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/wasm4pm-facts-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/affidavit-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/chicago-tdd-tools-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/repo-as-found-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/repo-load-path-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/repo-intervention-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/repo-reconciliation-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/temporary-works-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/claude-code-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/gh-terraform-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/mfw-pcp-level5-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP DECLARED_PARTIAL packs/ma-case-study-pack: standing is intentionally below PACK_WITNESS and requires a future executable witness
GAP FIELD_NOT_CROWN_COMPLETE I. Whole Manufacturing System — 1–12: **PARTIAL** at whole-system scale
GAP FIELD_NOT_CROWN_COMPLETE II. Pack as a Living Part — 13–24: **PARTIAL**; identity/passport work is implemented in marketplace
GAP FIELD_NOT_CROWN_COMPLETE IV. Standing before Scale — 37–52: **PACK_WITNESS** for several maturity cells; universal L5 is **TARGET**
GAP FIELD_NOT_CROWN_COMPLETE V. Independent Reality — 53–64: **PARTIAL**
GAP FIELD_NOT_CROWN_COMPLETE IX. Complete Product Surfaces — 121–134: **PARTIAL**; full multi-target substitution is not universally verified
GAP FIELD_NOT_CROWN_COMPLETE XII. Pack Neighborhood — 161–172: **PARTIAL**
GAP FIELD_NOT_CROWN_COMPLETE XIII. Time, Change and Repair — 173–186: **PACK_WITNESS** for checksum/idempotency; longitudinal repair is **PARTIAL**
GAP FIELD_NOT_CROWN_COMPLETE XIV. Receipts and Standing — 187–200: **PARTIAL** across the full supply chain
GAP FIELD_NOT_CROWN_COMPLETE XVI. Level Five Sequence — 213–224: **PARTIAL**
GAP FIELD_NOT_CROWN_COMPLETE XVII. TCPS Complete Pattern — 225–302: **PARTIAL** until current-head full toolchain replay
GAP FIELD_NOT_CROWN_COMPLETE XVIII. Make a New Language — 303–320: **TARGET**
GAP FIELD_NOT_CROWN_COMPLETE XIX. Certification Laboratories — 321–336: **PARTIAL** until all laboratories execute on current head
CLAIM_COVERAGE pack_rows=17 field_rows=21 chapter_files=366 chapter_alignment_sections=366 evidence_paths=49 consumer_witnesses=8 declared_gaps=25 errors=0 gaps=25
```

## TOML corpus

- Files parsed: **74**
- Failures: **0**

## Turtle corpus

- Files parsed: **213**
- Triples admitted from valid files: **58111**
- Invalid files: **11**
- `examples/archive/_validation_rules.ttl: at line 15 of <>:
Bad syntax (Prefix ":" not bound) at ^ in:
"...b'==========================================================\n\n'^b':ModelShape a sh:NodeShape ;\n    sh:targetClass ex:Model ;\n '..."`
- `examples/archive/bree-semantic-scheduler/bree-paas-generation.ttl: at line 28 of <>:
Bad syntax (Prefix ":" not bound) at ^ in:
"...b'ocker-compose.yml from PaaS ontology for local development\n\n'^b':GenerateDockerCompose\n  a bree:ScheduledJob ;\n  rdfs:label '..."`
- `examples/archive/comprehensive-rust-showcase/data/domain.ttl: at line 296 of <>:
Bad syntax (objectList expected) at ^ in:
"...b'ules [\n        "User email must be unique across the system"'^b',\n        "User name must be between 2 and 100 characters",\n'..."`
- `examples/archive/event-horizon/02-data-model/rdf-first/product-catalog.ttl: at line 220 of <>:
Bad syntax (objectList expected) at ^ in:
"...b'ange xsd:string ;\n    :rustType "Currency" ;  # Enum\n\n:hasQu'^b'antity a owl:DatatypeProperty ;\n    rdfs:domain :Inventory ;'..."`
- `examples/archive/factory-paas/templates/otel_sparql_queries.ttl: at line 267 of <>:
Bad syntax (Prefix "affiliate:" not bound) at ^ in:
"...b'tatus 201 .\n\n# Affiliate business event\ndomain:ClickEvent a '^b'affiliate:AffiliateEvent ;\n    domain:name "ClickTracked" ;\n'..."`
- `examples/archive/fastapi-from-rdf/domain.ttl: at line 169 of <>:
Bad syntax (expected item in list or ')') at ^ in:
"...b' ;\n    api:maxLength 100 ;\n    api:enumValues ("electronics"'^b', "clothing", "books", "food", "other") ;\n    api:descriptio'..."`
- `examples/archive/gcp-erlang-autonomics/.specify/specs/010-erlang-autonomic-c4/c4-components.ttl: at line 249 of <>:
Bad syntax (expected item in list or ')') at ^ in:
"...b'warn AND cpu_trend=increasing) then scale_up_by_20_percent" '^b';\n        "If (state=intervene AND deployment_age<1h) then r'..."`
- `examples/archive/gcp-erlang-autonomics/.specify/specs/010-erlang-autonomic-c4/c4-deployment.ttl: at line 429 of <>:
Bad syntax (expected item in list or ')') at ^ in:
"...b';\n\n    ea:modules (\n        "tf-modules/cloud-run-services" '^b';\n        "tf-modules/pubsub-topics" ;\n        "tf-modules/d'..."`
- `examples/archive/gcp-erlang-autonomics/.specify/specs/010-erlang-autonomic-c4/sku-mapping.ttl: at line 102 of <>:
Bad syntax (expected item in list or ')') at ^ in:
"...b'"2026-01-25" ;\n\n    sku:policies (\n        ea:CostCCBPolicy1'^b',\n        ea:CostCCBPolicy2,\n        ea:CostCCBPolicy3,\n    '..."`
- `examples/archive/maturity-matrix-showcase/level2-small/ontology.ttl: at line 45 of <>:
Bad syntax (Prefix "xsd:" not bound) at ^ in:
"...b' rdf:Property ;\n    rdfs:domain ex:Product ;\n    rdfs:range '^b'xsd:decimal ;\n    rdfs:label "Product Price" .\n\nex:productCa'..."`
- `examples/archive/self-play/ontology.ttl: at line 117 of <>:
Bad syntax (objectList expected) at ^ in:
"...b';\n    ggen:outputDirectory "target/self-play/" ;\n    ggen:de'^b'pendencies [\n        "rustc 1.91.1",\n        "cargo",\n      '..."`

## Generated shell laboratory syntax

- Scripts checked: **51**
- Syntax failures: **0**

## Discoverable consumer manifests

- Consumers found: **50**
- `examples/affidavit-verify`
- `examples/archive/a2a-agent-definition`
- `examples/archive/a2a-groq-agent`
- `examples/archive/advanced-rust-project`
- `examples/archive/ai-code-generation`
- `examples/archive/ai-template-project/test-project`
- `examples/archive/clap-noun-verb-demo`
- `examples/archive/cli-noun-verb`
- `examples/archive/comprehensive-rust-showcase`
- `examples/archive/config-generator`
- `examples/archive/database-schema`
- `examples/archive/factory-paas`
- `examples/archive/gcp-erlang-autonomics`
- `examples/archive/graphql-schema`
- `examples/archive/grpc-service`
- `examples/archive/llm-full-integration`
- `examples/archive/llm-skill-generation`
- `examples/archive/mcp-server-definition`
- `examples/archive/microservices-architecture`
- `examples/archive/middleware-stack`
- `examples/archive/nextjs-openapi-sqlite-shadcn-vitest`
- `examples/archive/observable-agent`
- `examples/archive/openapi`
- `examples/archive/openapi-variants`
- `examples/archive/rest-api-advanced`
- `examples/archive/rust-structs`
- `examples/archive/self-play`
- `examples/archive/simple-project`
- `examples/archive/thesis-gen`
- `examples/archive/validation-schemas`
- `examples/archive/weaver-semantic-conventions`
- `examples/archive/workspace-project/crates/cli`
- `examples/archive/workspace-project/crates/core`
- `examples/archive/workspace-project/crates/utils`
- `examples/archive/workspace-project`
- `examples/archive/yawl-workflow-platform`
- `examples/archive_ggen_core/ai-microservice`
- `examples/archive_ggen_core/mcp-a2a-self-hosting`
- `examples/cargo-cicd-verify`
- `examples/clap-noun-verb-cli`
- `examples/crown-conjecture-verify`
- `examples/lsp-max-verify`
- `examples/part-passport`
- `examples/praxis-core-verify`
- `examples/receiptctl`
- `examples/rmcp-verify`
- `examples/star-toml-verify`
- `examples/tcps-generated`
- `examples/tpot2-wasm4pm-autoconfig`
- `examples/wasm4pm-verify`

## Observed standing

- Hard corpus failures: **11**
- Standing is `GAPS_EXPOSED` until every hard failure is repaired or explicitly quarantined with a bounded rationale.
