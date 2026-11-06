# Lanes — v26.11.4 crash-recovery wave (2026-11-05)

Coordinator owns all git transitions; lanes never commit. One lane per repo
except AF-1 (autofde-lab + gym-ecosystem, doc-only).

| lane | repo | scope |
|---|---|---|
| IG-1 | /Users/sac/ggen_igniter | WIP-finish ~98 dirty files; mix compile + touched tests |
| SM-1 | /Users/sac/smalt | WIP-finish ~87 dirty files; mix compile + touched tests/courts |
| W4-1 | /Users/sac/wasm4pm | WIP-finish ~54 dirty fixture/code files; cargo test |
| CG-1 | /Users/sac/cgap | WIP-finish 20 dirty + untracked test files; cargo test |
| AR-1 | /Users/sac/arcus | WIP-finish 22 dirty; go build/test |
| MK-1 | /Users/sac/ggen-marketplace | kafka-wire-pack + smalt-substrate-pack verification |
| A2-1 | /Users/sac/ash_a2a | CHANGELOG + research/erc inventory; mix compile |
| FP-1 | /Users/sac/ferroplan | staged workflows/tests + census script; cargo test |
| XA-1 | /Users/sac/xaas | hold_request + fulfill_hold_consequences; mix test |
| AF-1 | /Users/sac/autofde-lab + /Users/sac/gym-ecosystem | doc finish + submodule pointer census |
| GY-1 | /Users/sac/gymact | doc-hdit receipts JSONL validation |
| GG-1 | /Users/sac/ggen | closure-doc dangling-reference sweep |
| S-9 | /Users/sac/ggen | gate harness BEAM+posh boot path |
| S-12 | /Users/sac/ggen | scripts/receipt_lint.py + just lint-receipts |
| S-11 | /Users/sac/ggen | lane-registry.json + lane-lease-check hook |
| S-1 | /Users/sac/ggen | tag-push publish rehearsal (dry-run only) |
| S-6 | /Users/sac/ggen | verify_v26.10.31.sh sed portability |
| S-7 | /Users/sac/ggen | E-2 observation re-pin gate |
| S-8 | /Users/sac/ggen-marketplace | pack re-verify at v26.11.3 tag |
| OI-2 | /Users/sac/ggen | tera 2.4.0 real dev-dep + render-diff court |

Disjoint-file notes: S-9/S-12/S-11/S-1/S-6/S-7/OI-2 all touch /Users/sac/ggen
but own disjoint files (gates/ vs scripts/receipt_lint.py vs
scripts/hooks/ vs docs receipts). Receipts: docs/jira/v26.11.4/receipts/
per lane. Open coordinator items: S-3 (ZP-21/ZP-14 home dispute) — decide at
drain from receipts.

## Wave 2 additions (slot refills, 2026-11-05)

| lane | repo | scope |
|---|---|---|
| S-4 | /Users/sac/cgap | ZP-15 foreign-subject projection court (new files only, CG-1 owns dirty set) |
| S-5 | /Users/sac/smalt | ZP-19 WindowBuffer soak (new files only, SM-1 owns dirty set) |
| OI-3 | /Users/sac/ggen | ML-DSA-65 phase 1: FIPS-204 witness fixtures + loader court |

Queue: OI-4 (zd-79 stress/chaos, BLOCKED — unblock attempt on next slot).
Closed this wave: MK-1 (PASS), GG-1 (PASS), GY-1 (PASS), S-3/OI-6
(coordinator decision recorded in open-items-register.md).
