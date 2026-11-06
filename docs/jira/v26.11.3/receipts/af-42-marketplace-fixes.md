# AF-42: ggen-marketplace fixes — cgap witness court RED, tort-pack tracking, qualification entries

Repo: /Users/sac/ggen-marketplace (canonical checkout, no worktree). Receipt filed in ggen's
receipts tree per AF-42 dispatch (ggen-marketplace has no docs/jira/v26.11.3 tree).

## Blocker 1 — cgap-pack witness court RED (arrow_layout)

- RED witness: `python3 scripts/check_gate_witness_courts.py` exited 2 with
  `REFUSED:GATE_WITNESS_COURT:cgap-pack: case correspondence failure: {"missing_fail": ["arrow_layout"], "missing_pass": ["arrow_layout"], ...}`
- Authored `packs/cgap-pack/witnesses/pass/arrow_layout.ttl` (ImportFn with fnSignature +
  fnErrorMapping, ArrayKind with formatCode/bufferCount/kindSlotWidths/kindAlignment, BitTestLaw
  with lawExpression — all three gate branches satisfied) and
  `packs/cgap-pack/witnesses/fail/arrow_layout.ttl` (same graph, single mutation: `p:kindSlotWidths`
  removed from the ArrayKind).
- **Real execution, not structural-only**: ran `arrow_layout.rq` via rdflib 7.6.0 against both
  witnesses — pass graph returns **0 rows**, fail graph returns **1 row** naming the mutated kind
  (`https://cgap.chatmangpt.com/witness_kind_ok`).
- GREEN: `python3 scripts/check_gate_witness_courts.py` exit 0 corpus-wide
  (50 configured packs, `configured_pack_count: 50`, all `standing: ALIVE`).

## Blocker 2 — tort-pack untracked

- Verified on disk: `git log -- packs/tort-pack/` empty, `git status --porcelain` shows
  `?? packs/tort-pack/`. It is genuinely untracked — the earlier "already committed and pushed"
  claim was stale (ERRC/tighten: stale claim retired with this receipt).
- `git add packs/tort-pack/` is left to the coordinator (lane law: never run git). Coordinator
  must include it in the integration commit.

## Blocker 3 — aoq/cscb/tort qualification entries

- Ran the real court: `python3 scripts/qualify_packs.py --pack <name>` for aoq-pack, cscb-pack,
  tort-pack — all exit 0, status ALIVE, profile=projection (cscb/tort receipts at
  /tmp/af42-cscb-pack.json, /tmp/af42-tort-pack.json).
- Appended three `qualification/baseline.json` entries with **court-run** consequence_sha256 +
  source_sha256 (provenance: qualify_packs.py court run, ggen 26.10.11 — stronger than the
  hand-computed fallback; not "not-a-court-run" labeled). baseline.json: 310 → 313 entries,
  re-parsed OK after write.

## Gates (all run this session)

- `python3 scripts/marketplace.py validate` → exit 0, "validated packs=347 manifests=347
  ontologies=575 templates=1958 native_gates=2015 verifier_gates=21".
- `python3 scripts/check_gate_witness_courts.py` → exit 0, 50 courts ALIVE.
- `qualification/baseline.json` parses, 313 pack entries.
- /tmp/af42-{aoq,cscb,tort}-pack.json court reports + /tmp/af42-validate.log preserved.

Standing: ALIVE — Falsifier for the fail witness: a gate with no rows on the mutated graph.
