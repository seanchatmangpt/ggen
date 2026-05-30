<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [MCPP Runtime & Control Substrate](#mcpp-runtime--control-substrate)
  - [Overview](#overview)
  - [Core Mandates](#core-mandates)
  - [Canonical Command Surface](#canonical-command-surface)
  - [Implementation Trace](#implementation-trace)
  - [Relationship to UniverseOS](#relationship-to-universeos)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# MCPP Runtime & Control Substrate

## Overview
**MCPP (Machine-grade Command and Protocol Substrate)** is the machine-level identity for the Model Context Protocol (MCP) execution layer within UniverseOS. While **MCP Plus** serves as the public project identity, MCPP provides the deterministic, ontology-bound control grammar for human and agent interaction.

The MCPP runtime is designed to move beyond "Old AI" (unconstrained, prompt-first interactions) toward a lawful, receipted execution model grounded in public ontologies.

## Core Mandates
1.  **A = μ(O*)**: All actions (A) are deterministic projections (μ) of a closed, semantically valid ontology (O*).
2.  **Public Ontology First**: MCPP is built on W3C/OBO standards:
    *   **ODRL**: Policy, permissions, and prohibitions.
    *   **SHACL**: Admissibility and structural validation.
    *   **PROV-O**: Provenance and execution history.
    *   **SPDX/CodeMeta**: Software artifact metadata and checksums.
3.  **Command Surface as Grammar**: The CLI is not a collection of flags but a formal control grammar using a `noun-verb` structure.

## Canonical Command Surface
MCPP defines three primary archetypes (Doctor, Wizard, Telco) as the baseline capabilities of any operational node:

| Command | Verb | Purpose |
|---------|------|---------|
| `mcpp doctor` | `check`, `diagnose` | Health checks and expert diagnostic surfaces. |
| `mcpp wizard` | `generate`, `regenerate` | Deterministic scaffold and artifact manufacturing. |
| `mcpp telco` | `route`, `bridge` | Message routing and agent-to-tool connectivity. |
| `mcpp spec` | `validate` | Validates Spec Kit RDF graphs against SHACL shapes. |
| `mcpp ontology` | `sync` | Synchronizes the local execution state with the public graph. |
| `mcpp receipt` | `verify` | Verifies cryptographic receipts of past actions. |
| `mcpp powl8` | `emit` | Lowers the high-level spec into executable POWL8 ISA micro-ops. |

## Implementation Trace
- **Configuration**: Managed via `mcpp.toml`, which acts as the operational anchor.
- **Protocol**: Bridges Agent-to-Agent (A2A) communication with RMCP.
- **Lowering**: Spec Kit (Constitution → Spec → Plan → Tasks) is projected into RDF and lowered into POWL8 for execution.
- **Closure**: All actions must terminate in a cryptographic receipt chain, proving behavioral and structural correctness.

## Relationship to UniverseOS
MCPP is the "hands and eyes" of UniverseOS. It provides the executable port through which the UniverseOS semantic brain interacts with the physical repository and the digital team (DTeam).
