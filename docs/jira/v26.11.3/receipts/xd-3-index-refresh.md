# XD-3 — Receipt-Index Refresh

Date: 2026-11-04. Lane: XD-3 (index-only). Repo: /Users/sac/ggen. Zero
src/test edits, zero git commands. Files touched (only):
`docs/jira/v26.11.3/receipts/cl-close-2.md` (addendum rows appended),
`docs/jira/v26.11.3/closure/v26.11.3-closure.md` (standing +
verification-ladder sections only), this receipt.

## Drift found

The canonical index surfaces (docs/jira/v26.11.3/README.md, last written
2026-11-03 18:38, and RECEIPTS.md, 18:27) do not carry any of the 12 ggen
receipts landed after their write:

sx-1-reds, af-7-path-reconcile, af-5-sync-surface, ab-14-chain-scale,
ga-11-cscb-pack, ab-13-kernel-stress, ga-12-tort-pack, af-42-marketplace-fixes,
gd-2-ggen-dod, xc-8-cve-bump, xc-7-ci-unblock, xd-2-key-perms.

Row content for all of them (standing words re-read from receipt bodies on
disk this session, exit records cited command+exit where the receipt
carries them; UNSTATED used where a body carries no standing word) was
written into the XD-3-owned surfaces: the cl-close-2.md addendum rows
section, cross-repo rows, and the closure seal's standing/ladder. The
README.md/RECEIPTS.md themselves are NOT owned by this lane and were not
edited — the rows are XD-3's until a successor lane transcribes them.

## Rows landed under this receipt

- 12 new ggen receipts — full table in cl-close-2.md addendum
  ("New ggen receipts since index last write").
- Cross-repo, sweep 1: BK-1 (AB-4/AB-5 ALIVE, AF1-KD-1 CLOSED, cgap-crypto
  75/0 ×2), AF-37 (ALIVE, p99 torture ×4 green + pre-existing doctest
  failure disclosed), RX-2 (ALIVE, conformance suite ×4 exit 0, lib 14/0),
  GD-1 (ALIVE pack surface / PARTIAL overall, dod-006 TARGET-UNMEASURED) —
  all hosted in /Users/sac/cgap/docs/jira/v26.11.3/receipts/, rowed
  pointer-only.
- Cross-repo, sweep 2 (landed mid-pass, disclosed): XD-1 (cgap, ALIVE,
  cargo-deny advisories 0 / licenses exit 1 all-internal unlicensed),
  XD-5 (cgap, ALIVE doc edits; attack-scenario population UNKNOWN), XD-4
  (arcus, ALIVE; govulncheck exit 3 at go1.23.4 → exit 0 pinned
  go1.26.10, wrapper exit 0).
- Not found on disk: XC-5r (named as the source census by XD-2 "rec #6"
  and XD-4 "rec #8" — expected but not landed anywhere in the six-repo
  receipts trees ls'd 2026-11-04) and DOCS-* (zero receipts or references
  under arcus or ggen v26.11.3 trees). Rowed UNVERIFIED-absent, not
  invented.

## Count claims (re-read at use time)

- `ls /Users/sac/ggen/docs/jira/v26.11.3/receipts/ | wc -l` → 89 at this
  receipt's write; 90 with this receipt on disk (this receipt is the 90th,
  self-subject).
- arcus 27, cgap 50 (ls'd same sweep).
- v26.11.3-closure.md's "71+ receipts" and cl-close-2.md's census figure
  (177 five-repo total) are superseded by these counts as of 2026-11-04.

## Standing

ALIVE for the XD-3 scope: every row written this session from receipt
bodies on disk (never memory), all counts from real `ls | wc -l` output,
all named-but-absent receipts recorded as absent rather than rowed.
Falsifier: any row's standing word or exit record disagrees with its
receipt body on disk; any count disagrees with `ls | wc -l`; README.md /
RECEIPTS.md gain the rows without discarding these (drift duplicated
rather than reconciled).

## See Also

- receipts/cl-close-2.md (the addendum rows this receipt lands)
- closure/v26.11.3-closure.md (standing/ladder refresh)
- /Users/sac/cgap/docs/jira/v26.11.3/receipts/ — bk-1-takeover.md,
  af-37-p99-torture.md, rx-2-conformance.md, gd-1-dod-pack.md,
  xd-1-deny-toml.md, xd-5-error-taxonomy.md
- /Users/sac/arcus/docs/jira/v26.11.3/receipts/xd-4-govulncheck.md
