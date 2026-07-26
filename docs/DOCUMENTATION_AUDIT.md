# Documentation Audit Ledger

```yaml
class: EVIDENCE
standing: PARTIAL_ALIVE
scope: documentation control plane and canonical entry points
source_revision: b82ea9ef848a5164dd6a09bb76539908fde41922
last_verified: 2026-07-24
owner: ggen core team
```

## Result

The repository contains strong evidence-first material, generated regions, claims ledgers, architecture rules, operational guides, plans, and archives. The primary defect is authority ambiguity: a reader cannot always determine which document is law, generated output, current observation, planning, or history.

This change repairs the control plane and canonical entry points. It does not claim that every long-tail document has already been reconciled.

## Findings

| Finding | Standing | Consequence |
|---|---|---|
| `README.md` contains a generated/manual merge region. | `ALIVE` by source inspection | Its markers must be preserved. |
| The root README mixes overview, architecture, run evidence, limitations, maturity, and routing. | `ALIVE` by source inspection | The front door is accurate but overloaded. |
| `CLAUDE.md` combines repository rules, generated architecture, command audits, and historical migration detail. | `ALIVE` by source inspection | Normative law and revision-bound evidence need separation. |
| `docs/README.md` reports duplicated and archive-adjacent sections. | `ALIVE` by source inspection | Existing drift must be classified rather than hidden. |
| `docs/GETTING_STARTED.md` contains verified transcripts plus volatile versions, timings, counts, and machine paths. | `ALIVE` by source inspection | Tutorial and evidence-log concerns are coupled. |
| Executable claims and falsifiers already exist in the repository. | `PARTIAL_ALIVE` | The foundation exists but is not uniformly applied. |

## Authority map

1. Machine-enforced guards, schemas, and verifier code.
2. Admitted ontologies/specifications and generated projections.
3. Claims ledgers with executable falsifiers.
4. Normative core-team documents.
5. Verified operational guides.
6. Current reference documentation.
7. Revision-bound evidence reports.
8. Planning and historical material.

## Changed in this branch

- Added the core-team documentation standard.
- Added this audit ledger.
- Reworked the root documentation routing.
- Reworked the documentation index front door.

## Bounded follow-through

The remaining corpus must be migrated by class:

- Generated files: edit the source and regenerate; do not hand-edit projections.
- Operational guides: execute commands at the PR head before assigning `ALIVE`.
- Architecture references: reconcile against generated crate and ownership maps.
- Pack guides: preserve local proof gates and avoid promoting partial checks to full maturity.
- Planning trees: classify as executed, superseded, blocked, or historical.
- Examples and archives: retain as current evidence only when they remain on a verified user path.

Their aggregate standing remains `UNKNOWN` until a complete inventory and applicable verification are observed.

## Migration protocol

For every maintained document:

1. Parse claims, commands, links, ownership, and generated boundaries.
2. Assign one documentation class.
3. Admit current facts from source, execution, or claims ledger.
4. Remove or refuse stale and unsupported claims.
5. Rewrite against the canonical hierarchy.
6. Run the narrowest applicable verifier.
7. Record standing and exclusions.
8. Regenerate projections when required.
9. Run repository documentation gates.
10. Attach the resulting receipt to the pull request.

## Required verification

The corpus cannot reach `ALIVE` until these repository gates execute successfully at the PR head:

```bash
bash scripts/validate-readme.sh
bash scripts/validate-readme-claims.sh
python3 book/scripts/check_book.py
just test-doc
just pre-commit
```

## Current standing

`PARTIAL_ALIVE`

The documentation control plane is established. A repository-wide `ALIVE` claim remains falsified until all maintained documents are classified and the required gates pass at the final head revision.