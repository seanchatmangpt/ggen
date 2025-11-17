# Evidence Graph 3.0 - Summary Report

Generated: 2025-11-17 01:26:57 UTC

## Overview

- **Total Nodes**: 198
  - Concepts: 39
  - Systems: 9
  - Evidence: 150
- **Total Edges**: 119

## Edge Types

- **composed_with**: 5
- **decomposes**: 22
- **depends_on**: 4
- **enables**: 2
- **enforces**: 1
- **implements**: 30
- **proves**: 1
- **refines**: 4
- **requires**: 1
- **supports**: 47
- **validates**: 2

## Evidence Types

- **implementation**: 55
- **test**: 18
- **quantitative**: 18
- **code_comment**: 13
- **function_signature**: 9
- **verification**: 5
- **specification**: 5
- **documentation**: 4
- **data_structure**: 4
- **enforcement**: 3
- **constant_definition**: 2
- **trait_definition**: 2
- **architecture**: 1
- **error_type**: 1
- **statistical_analysis**: 1
- **invariant_definition**: 1
- **trait_method**: 1
- **module_definition**: 1
- **integration_test**: 1
- **state_machine**: 1
- **state_transitions**: 1
- **re_export**: 1
- **module_import**: 1
- **guarantee**: 1

## Coverage Metrics

- **Concepts with Evidence**: 17/39
- **Average Concept Strength**: 0.909
- **Percent Direct Evidence**: 94.0%

## Top Concepts by Evidence Count

### C_GRAPH_UNIVERSE_PRIMARY
- Evidence Count: 4
- Implicit Evidence: 8
- Granular Claims: 0
- Avg Strength: 0.96
- Min/Max Strength: 0.90 / 1.00

### C_RECEIPTS_AND_PROOFS
- Evidence Count: 4
- Implicit Evidence: 5
- Granular Claims: 10
- Avg Strength: 0.96
- Min/Max Strength: 0.90 / 1.00

### C_CODE_AS_PROJECTION
- Evidence Count: 3
- Implicit Evidence: 0
- Granular Claims: 0
- Avg Strength: 0.95
- Min/Max Strength: 0.90 / 1.00

### C_MU_KERNEL_PHYSICS
- Evidence Count: 3
- Implicit Evidence: 0
- Granular Claims: 0
- Avg Strength: 0.98
- Min/Max Strength: 0.95 / 1.00

### C_TIMING_BOUNDS_ENFORCED
- Evidence Count: 3
- Implicit Evidence: 11
- Granular Claims: 12
- Avg Strength: 0.98
- Min/Max Strength: 0.95 / 1.00

### C_KNHK_GRAPH_PRIMARY
- Evidence Count: 3
- Implicit Evidence: 0
- Granular Claims: 0
- Avg Strength: 0.82
- Min/Max Strength: 0.70 / 0.90

### C_AHI_GOVERNANCE
- Evidence Count: 3
- Implicit Evidence: 0
- Granular Claims: 10
- Avg Strength: 0.95
- Min/Max Strength: 0.90 / 1.00

### C_CTT_12_PHASE_VERIFICATION
- Evidence Count: 3
- Implicit Evidence: 0
- Granular Claims: 0
- Avg Strength: 0.85
- Min/Max Strength: 0.80 / 0.90

### C_GGEN_PROJECTION_ENGINE
- Evidence Count: 3
- Implicit Evidence: 1
- Granular Claims: 0
- Avg Strength: 0.92
- Min/Max Strength: 0.80 / 1.00

### C_UNIVERSE_PROJECTION_AXIOM
- Evidence Count: 3
- Implicit Evidence: 7
- Granular Claims: 9
- Avg Strength: 0.98
- Min/Max Strength: 0.95 / 1.00

## Key Architectural Insights

- **Timing Bounds**: CHATMAN_CONSTANT_MS = 8ms enforced at kernel level with fatal violations
- **Determinism**: SHA256 hash verification ensures A = μ(O) with replay testing
- **Cryptographic Proofs**: HMAC-SHA256 signatures provide tamper-proof receipts
- **Autonomous Governance**: AHI manages ΔΣ via MAPE-K loop without human arbitration
- **Ontology Primacy**: Σ (RDF ontologies) as single source of truth for all code generation
- **Complete Provenance**: Every decision tracked from observations → receipts → evidence chain
