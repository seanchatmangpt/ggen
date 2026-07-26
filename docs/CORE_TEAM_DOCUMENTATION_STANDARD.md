# Core-Team Documentation Standard

Status: **normative**  
Applies to: every maintained document, generated documentation surface, example, pack guide, and operator runbook in this repository.

## 1. Purpose

ggen documentation is an executable interface to the system, not a narrative afterthought. A document has standing only when a reader can determine:

1. what object or capability it governs;
2. which repository state it describes;
3. whether the claim is normative, observed, inferred, planned, or historical;
4. how to falsify the claim;
5. what action is permitted or refused;
6. what receipt proves the stated consequence.

The governing lifecycle is:

```text
parse → route → admit/refuse → diagnose/repair → actuate → receipt → replay/hook
```

The governing artifact equation is:

```text
A = μ(O*)
```

`O*` is admitted, aligned, complete-enough, grounded, and bounded observation. Documentation must never silently promote partial or stale observation into a system claim.

## 2. Documentation classes

Every maintained document must declare one class near its title.

| Class | Meaning | Required evidence |
|---|---|---|
| `NORMATIVE` | Defines repository law, invariants, interfaces, or contribution rules. | Source path, owner, change process, exclusions. |
| `OPERATIONAL` | Tells an operator how to perform and verify a task. | Copy-paste commands, expected exit state, rollback/refusal behavior. |
| `REFERENCE` | Describes current structures, schemas, commands, or APIs. | Generated or source-linked facts and a freshness boundary. |
| `EVIDENCE` | Records an observed run, benchmark, audit, or verifier result. | Exact revision, command, environment boundary, raw receipt location. |
| `DECISION` | Captures an accepted architectural or product decision. | Context, decision, alternatives, consequences, supersession rule. |
| `PLANNING` | Describes intended work without claiming implementation. | Explicit `PLANNED` standing and acceptance/falsifier. |
| `HISTORICAL` | Preserves superseded material. | Superseded-by pointer; no live instructions. |
| `GENERATED` | Is projected from an admitted source. | Generator, source of truth, regeneration command, no-hand-edit warning. |

A document that cannot be classified is `UNKNOWN` and must not be linked as authoritative.

## 3. Claim standing

Use only these standing values:

- `ALIVE`: observed execution produced the stated consequence.
- `PARTIAL_ALIVE`: a bounded checkpoint passed; the full claim remains open.
- `BLOCKED`: an admitted dependency or capability prevents execution.
- `BUILD_BROKEN`: the repository cannot currently reach the relevant verifier.
- `UNKNOWN`: observation is absent, stale, or contradictory.
- `UNSUPPORTED`: the capability is outside the admitted system boundary.

Never use prose such as “works,” “complete,” “production-ready,” or “verified” without a standing, scope, revision, and falsifier.

## 4. Required document header

Maintained documents should begin with this compact contract:

```yaml
class: OPERATIONAL
standing: ALIVE
scope: ggen sync and receipt verification
source_revision: <git SHA or generated source>
last_verified: YYYY-MM-DD
owner: <subsystem or team>
falsifier: <command or verifier>
```

Generated files may express the same fields in their source ontology/template rather than hand-authored YAML.

## 5. Canonical hierarchy

Authority is resolved in this order:

1. machine-enforced repository law and schemas;
2. admitted ontologies/specifications and generated projections;
3. claims ledgers with executable falsifiers;
4. normative core-team documents;
5. operational guides verified against the current revision;
6. reference documentation;
7. evidence snapshots;
8. planning and historical material.

When two documents disagree, the lower-authority document must not reconcile the conflict rhetorically. It must identify the conflicting objects, classify the standing, and point to the owning source.

## 6. Single-source rules

- Generated documentation is edited at its admitted source, never at the projection.
- A command contract has one canonical reference. Tutorials link to it rather than reproducing mutable option lists.
- A maturity claim lives in the claims ledger; prose summarizes it and names its falsifier.
- Architecture ownership lives with the subsystem/source map; other documents project task-specific views.
- Historical transcripts never define current behavior.
- Counts, versions, benchmark timings, and command output are snapshots unless generated during verification.

## 7. Operational guide structure

An operational guide uses this order:

1. **Outcome** — the observable state produced.
2. **Boundary** — prerequisites, permissions, destructive surfaces, and unsupported cases.
3. **Admission** — checks that must pass before actuation.
4. **Procedure** — minimal commands in execution order.
5. **Receipt** — output, hashes, files, or verifier report proving consequence.
6. **Replay** — how to reproduce the result.
7. **Failure map** — typed failures, repair path, and stop conditions.
8. **Rollback** — how to restore the prior state when actuation is reversible.

## 8. Architecture and design documents

Architecture documentation must identify:

- objects and boundaries;
- allowed morphisms/data flows;
- admission and refusal points;
- closure assumptions;
- actuation authority;
- receipt and replay path;
- explicit exclusions;
- a falsifier or verifier ladder.

Adjacency is not equivalence. A neighboring subsystem, library, or paper does not refute or replace ggen behavior unless it matches the same objects, morphisms, admission, closure, actuation, receipt/replay, boundary, and exclusions.

## 9. Verification ladder

Documentation claims should climb only as high as observed evidence permits:

```text
unit → integration → end-to-end → chaos → stress → benchmark → verifier report
```

A lower rung can establish `PARTIAL_ALIVE`; it cannot establish the crown claim. `ALIVE` requires an observed run at the rung named by the claim.

## 10. Examples and commands

- Commands must be runnable from a named working directory.
- Include the expected exit code or standing.
- Do not use machine-specific absolute paths.
- Use placeholders only when their substitution rule is explicit.
- Distinguish safe inspection/dry-run commands from actuation.
- Destructive or overwrite behavior must be fenced before the command.
- Output excerpts must state whether they are exact, abbreviated, or schematic.

## 11. Language rules

Prefer direct system language:

- “The verifier exits 0 at revision X” over “This should work.”
- “The command is unsupported” over “The command may not be available.”
- “No execution evidence was admitted” over “Unverified.”
- “Edit `.specify/repo-facts.ttl`; regenerate the projection” over “Update both files.”

Avoid promotional superlatives, anthropomorphic claims, speculative capability inflation, and hidden assumptions about AGI or human review. “Post-AGI” means the documentation must remain operable by mixed human/agent core teams under high change velocity, not that unobserved intelligence substitutes for evidence.

## 12. Pull-request receipt

A documentation PR must report:

- exact base and head revisions;
- files/classes changed;
- claims added, changed, or retired;
- commands or link checks executed;
- generated surfaces regenerated or deliberately untouched;
- unresolved `UNKNOWN`, `BLOCKED`, or `BUILD_BROKEN` items;
- the next falsifier that would change standing.

Zero unreceipted actuation applies to documentation changes.