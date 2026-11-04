# marketplace-index-bundle-ruling

## Ruling

The 10 unindexed directories under `marketplace/packages/` are TEMPLATE BUNDLE
dirs, not standalone ggen packages. They are legitimately out of the per-package
index (`marketplace/index.json`). Do NOT regenerate `index.json` to include
them.

## Evidence (per-dir shape, re-verified 2026-11-04)

- api-gateway-service-mesh: no root manifest; ontology/queries/src/tests only.
- chatman-businessos-platform: ggen.toml AT BUNDLE ROOT (see re-check trigger).
- compliance-audit-system: no root manifest; rdf/sparql/src/tests.
- data-warehouse-etl: no root ggen.toml; ontology/queries only.
- healthcare: no root manifest; sub-packages (dicom-medical-imaging,
  fhir-patient-management, hl7-v2-integration) + verify_packages.sh.
- identity-access-management: no root manifest; ontology/queries.
- notification-messaging-hub: no root ggen.toml; package.json + rdf/sparql/src.
- observability-platform: no root manifest; ontology/queries.
- payment-admission: bare package.toml (not ggen.toml); ontology only.
- workflow-automation-engine: no root manifest; ontology/queries.

Index census at ruling time: `marketplace/index.json` = 76 entries; all 10 dirs
confirmed absent from the name set (measured, not recalled).

## Falsifier / re-check trigger

A bundle dir that gains a root `ggen.toml` and ships standalone must then be
indexed. Re-check trigger: any of the 10 dirs gaining a root `ggen.toml`
becomes index-eligible and must be re-ruled or indexed. As of this ruling
exactly one dir already carries a root ggen.toml
(chatman-businessos-platform) and remains unindexed — it stays unindexed
under this ruling only until it ships standalone; next index-touch must
re-rule it explicitly.

## Standing

DECIDED (coordinator decision, 2026-11-04).
