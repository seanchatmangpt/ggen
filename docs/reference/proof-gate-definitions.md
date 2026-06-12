<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Reference: Manufacturing Proof Gates](#reference-manufacturing-proof-gates)
  - [O-01: Schema Valid](#o-01-schema-valid)
  - [O-02: Ontology Lawful](#o-02-ontology-lawful)
  - [M-01: Projection Complete](#m-01-projection-complete)
  - [M-02: Compilation Passes](#m-02-compilation-passes)
  - [P-01: Receipt Valid](#p-01-receipt-valid)
  - [O-03: Ethos Conformant](#o-03-ethos-conformant)
  - [T-01: Observability Present](#t-01-observability-present)
  - [C-01: Causal Consistent](#c-01-causal-consistent)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Reference: Manufacturing Proof Gates

Detailed specification of the validation criteria for each proof gate in the `ggen` v26.5.19 release.

## O-01: Schema Valid
- **Requirement**: `NormalizationPass` (μ₁) must have `success: true`.
- **Validation**: Ensures the ontology parsed correctly and all prefix namespaces are registered in the project whitelist.

## O-02: Ontology Lawful
- **Requirement**: `ExtractionPass` (μ₂) must have `success: true`.
- **Validation**: Checks for SPARQL query errors and verifies that the extracted model has no "Illegal State" transitions.

## M-01: Projection Complete
- **Requirement**: `receipt.outputs` must not be empty.
- **Validation**: Fails if the pipeline ran but produced zero physical files.

## M-02: Compilation Passes
- **Requirement**: `CanonicalizationPass` (μ₄) must have `success: true`.
- **Validation**: Fails if `rustfmt` (or target language equivalent) returns a non-zero exit code on the generated files.

## P-01: Receipt Valid
- **Requirement**: `receipt.is_valid` is true and `receipt.id` is non-empty.
- **Validation**: Fails if the internal SHA-256 verification of the receipt file fails.

## O-03: Ethos Conformant
- **Requirement**: `ManufacturingIntent.objective` must be a non-empty string.
- **Validation**: Ensures that the developer has declared a purpose for the manufacturing run.

## T-01: Observability Present
- **Requirement**: `receipt.total_duration_ms > 0` and all pass durations are `> 0`.
- **Validation**: Proves that the system had enough "presence" to record its own execution time.

## C-01: Causal Consistent
- **Requirement**: `ontology_hash` and `outputs_hash` must exist.
- **Validation**: Confirms that the artifact is a direct descendant of the specific ontology version.
