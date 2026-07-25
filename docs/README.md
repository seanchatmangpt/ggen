# ggen Documentation

```yaml
class: REFERENCE
standing: PARTIAL_ALIVE
scope: documentation routing and authority
owner: ggen core team
```

This directory is the documentation control plane for ggen. Route by task and authority; do not select a document merely because its title appears relevant.

## Start by outcome

| Outcome | Canonical document |
|---|---|
| Understand ggen's system boundary and first build | [../README.md](../README.md) |
| Build, run a sync, verify its receipt, and replay the result | [GETTING_STARTED.md](GETTING_STARTED.md) |
| Resolve implementation questions and known caveats | [FAQ.md](FAQ.md) |
| Determine whether a capability currently has standing | [aps/README.md](aps/README.md) and `aps/claims.toml` |
| Apply documentation law or review a documentation change | [CORE_TEAM_DOCUMENTATION_STANDARD.md](CORE_TEAM_DOCUMENTATION_STANDARD.md) |
| Inspect the current documentation audit and migration boundary | [DOCUMENTATION_AUDIT.md](DOCUMENTATION_AUDIT.md) |
| Navigate the complete long-lived tree | [INDEX.md](INDEX.md) |
| Contribute code or documentation | [../CONTRIBUTING.md](../CONTRIBUTING.md) |

## Authority order

When documents overlap or conflict, resolve authority in this order:

1. machine-enforced guards, schemas, and verifier code;
2. admitted ontologies/specifications and generated projections;
3. claims ledgers with executable falsifiers;
4. normative core-team documents;
5. verified operational guides;
6. current references;
7. revision-bound evidence reports;
8. planning and historical material.

A lower-authority document must not reconcile a conflict by rhetoric. Identify the conflicting objects, preserve both observations, classify standing, and route to the owning source.

## Documentation classes

Maintained documentation is classified as one of:

- `NORMATIVE` — repository law, invariants, interfaces, or contribution rules;
- `OPERATIONAL` — executable procedures with admission, receipt, replay, and failure behavior;
- `REFERENCE` — current structures, schemas, commands, or APIs;
- `EVIDENCE` — revision-bound runs, audits, benchmarks, or verifier reports;
- `DECISION` — accepted architecture or product decisions;
- `PLANNING` — intended work that does not claim implementation;
- `HISTORICAL` — superseded material preserved for provenance;
- `GENERATED` — projections edited only through their admitted source.

The full contract is defined in [CORE_TEAM_DOCUMENTATION_STANDARD.md](CORE_TEAM_DOCUMENTATION_STANDARD.md).

## System and architecture

| Surface | Purpose |
|---|---|
| `../CLAUDE.md` | Repository implementation doctrine, enforced boundaries, generated architecture summary, and core development rules. |
| `../.claude/rules/architecture.md` | Generated, detailed architecture projection. Edit its source ontology rather than the generated file. |
| [reference/workspace/crates.md](reference/workspace/crates.md) | Lightweight workspace crate reference. |
| [reference/ggen_sync_manual.md](reference/ggen_sync_manual.md) | Sync command reference. |
| [marketplace/ARCHITECTURE.md](marketplace/ARCHITECTURE.md) | Pack and marketplace architecture. |

## Verification and standing

| Surface | Purpose |
|---|---|
| [aps/README.md](aps/README.md) | Claims-ledger model and usage. |
| `aps/claims.toml` | Claim → falsifier → evidence → standing records. |
| [PERFORMANCE_QUICK_START.md](PERFORMANCE_QUICK_START.md) | Current automated performance path and its admitted limitations. |
| [performance/README.md](performance/README.md) | Broader performance documentation, including explicitly aspirational targets. |
| [l5-promotion/L5_PROMOTION_PROGRAM.md](l5-promotion/L5_PROMOTION_PROGRAM.md) | Per-capability Level-5 pack promotion program. |
| [packs/PACK_MATURITY_MODEL.md](packs/PACK_MATURITY_MODEL.md) | Sibling pack-maturity calibration; reconcile disagreements before asserting current maturity. |

## Agents and operators

- [agent/README.md](agent/README.md) routes agent implementation guidance.
- [GETTING_STARTED.md](GETTING_STARTED.md) is the canonical operator path for source build, sync, receipt verification, and replay.
- [FAQ.md](FAQ.md) records grounded answers and implementation caveats that do not belong in the front-door overview.

## Long-lived tree

`docs/` contains architecture, pack, research, ADR, planning, evidence, and historical trees accumulated across multiple repository generations. [INDEX.md](INDEX.md) is the directory map, but a directory listing is not proof of currency.

Known taxonomy conflicts include archive-adjacent content and near-duplicate trees such as `explanations/` versus `explanation/` and `how-to-guides/` versus `how-to/`. Until each surface is classified and verified, its aggregate standing is `UNKNOWN`; this does not assert that the content is false.

## Reader safety

Before executing instructions from any document, determine:

1. its class and standing;
2. the revision or generated source it describes;
3. whether the command inspects or actuates;
4. its prerequisites and refusal conditions;
5. the receipt or verifier that proves the consequence;
6. whether a higher-authority source supersedes it.

For the current audit boundary and corpus migration protocol, read [DOCUMENTATION_AUDIT.md](DOCUMENTATION_AUDIT.md).