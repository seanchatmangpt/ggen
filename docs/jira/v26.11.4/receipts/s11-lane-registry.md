# S-11 Lane Registry / Lane-Lease Pre-Commit Gate Receipt

Standing: ALIVE

- **Lane**: S-11 (v26.11.4 backlog seed, mechanical-gates-spec Gate 1 — adopt, not re-spec)
- **Repo**: /Users/sac/ggen (canonical checkout, no worktree)
- **Composed with**: O3's landed `scripts/lane_lease_hook.py` (Gate 1 engine, SD-19).
  S-11 delivers the spec-named enforcement-point shim, the slot-map projection, the
  S-11 lease row, and the Gate 1 falsification courts. No engine re-implementation.

## Files owned (all new/modified by this lane)

- `scripts/hooks/lane-lease-check` — spec-named pre-commit entry point; execs
  `scripts/lane_lease_hook.py` (same env contract: `GGEN_LANE_ID`,
  `GGEN_LANE_DISCLOSE`, `GGEN_LANE_REGISTRY`). chmod +x set.
- `scripts/lane_slot_map.sh` — read-only projection of
  `docs/jira/*/lane-registry.json` into a lane/status/lease/owned-paths table.
- `docs/jira/v26.11.4/lane-registry.json` — added the coordinator-shaped `S-11`
  lease row (owned_paths = the three S-11 files, status `leased`,
  lease_expires 2026-11-06T00:00Z). DISCLOSED: registry is coordinator-minted;
  S-11 row self-registered so the gate has a lawful lease to check against.

## Falsification (synthetic staged index, GIT_INDEX_FILE=/tmp/s11_index, cleaned up after)

Real git index: built via `git read-tree HEAD` + `git update-index --cacheinfo`
into a temp index file (real index untouched — verified below). Foreign probe
staged `crates/ggen-cli/src/foreign_probe.rs` (outside S-11 globs); owned probe
staged `scripts/lane_slot_map.sh`.

| # | Court | Command | Expected | Actual | Exit |
|---|---|---|---|---|---|
| T1 | foreign staged path refuses | `GGEN_LANE_ID=S-11 sh scripts/hooks/lane-lease-check` | refuse + owner | `REFUSED — lane 'S-11' staged paths outside its lease: crates/ggen-cli/src/foreign_probe.rs (owner: UNOWNED)` | 1 |
| T2 | no lease in env refuses | unset `GGEN_LANE_ID`, same hook | refuse | `REFUSED — no lane lease in environment; commits are coordinator-only` | 1 |
| T3 | COORD bypass, owned-only | `GGEN_LANE_ID=COORD ...` | pass | (silent) | 0 |
| T4 | owned path passes | `GGEN_LANE_ID=S-11 ...` | pass | (silent) | 0 |
| T5 | stale lease refuses | `GGEN_LANE_REGISTRY=/tmp/s11_stale_registry.json` (lease_expires 2026-11-01) | refuse | `REFUSED — lease for 'S-11' expired at 2026-11-01T00:00Z` | 1 |
| T6 | COORD orphan audit refuses | foreign path staged + `GGEN_LANE_ID=COORD` | refuse | `REFUSED — orphan staged path (no lane owns it)` | 1 |
| T7 | disclose-adopt passes w/ footer | `GGEN_LANE_DISCLOSE=S-11` | footer + 0 | `Index-disclosure: foreign paths committed under S-11 lease: ...` | 0 |

Cleanup: temp index, stale-registry copy, probe files all removed (`rm -f
/tmp/s11_index /tmp/s11_stale_registry.json /tmp/s11_foreign_probe.txt ...`);
`git status` shows the real tree carrying only this lane's owned files plus
pre-existing other-lane modifications (untouched).

## Environment note (pre-existing, not introduced by this lane)

Direct exec of any shebang script (`./script`, even a 2-line `echo hi` probe)
hangs in this sandbox (`timeout` fires, exit 124); invoking via explicit
interpreter (`sh script`, `python3 script`) works. Courts T1–T7 therefore run
the hook via `sh scripts/hooks/lane-lease-check` — same argv contract the
engine receives. Operator may prefer the `python3 scripts/lane_lease_hook.py`
form when wiring `.git/hooks/pre-commit`.

## Gate summary

Hook exits 1 on foreign path (T1), 0 on owned path (T4); stale lease, empty
lease env, and COORD orphan audit all exit 1. No real commit was made.

## See Also

`docs/jira/v26.11.4/closure/mechanical-gates-spec.md` (Gate 1) ·
`docs/jira/v26.11.4/lane-registry.json` · `scripts/lane_lease_hook.py`
