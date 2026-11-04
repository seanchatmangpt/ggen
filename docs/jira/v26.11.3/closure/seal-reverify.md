# Seal re-verify receipt — PF-1 falsifier ladder over the v26.11.3 closure seal

Date: 2026-11-04. Lane: verification lane (create-only; no other file touched,
no git state changed). Subject: the four falsifier classes of
`v26.11.3-closure.md` re-checked against the six canonical checkouts at their
current state. Seal SHAs are treated as as-of-seal-time; ancestor checks only;
tip drift disclosed, not voiding.

## Method

- Existence: `git cat-file -t <sha>`; ancestry: `git merge-base --is-ancestor
  <sha> <claimed base>` (exit 0 = yes). Commands run live this session.
- Standing spot-check: read each cited receipt, take the standing token from
  the receipt's own Standing section (first match may fall inside a quoted
  court string — context-checked by reading the body, not the grep line).
- Counts: `ls docs/jira/v26.11.3/receipts | wc -l` fresh this session.

## Falsifier class 1 — recorded commit SHAs exist and are ancestors where claimed

| repo | recorded SHA | exists | ancestor of claimed base | verdict |
|---|---|---|---|---|
| ggen | c24d71b61 (v26-11-3-wave, "pushed; PR #824 head") | commit | v26-11-3-wave: yes | PASS |
| cgap | 7eef0938 (main, pushed) | commit | main: yes | PASS |
| claude-code-config-lsp | 23079fd (main, pushed) | commit | main: yes | PASS |
| ggen-marketplace | 37dc9febc (recorded "main") | commit | main: yes; also current HEAD (zd-26-frozen-drift): yes | DRIFT-DISCLOSED (checkout has since moved to branch zd-26-frozen-drift at e7c58d5de; recorded SHA still ancestral) |
| lsp-max | c7fde33d (master, "LOCAL, push deferred") | NOT FOUND (`git cat-file -t` fatal) | n/a | FAIL — with receipted supersession (see note) |
| arcus | 03ae116 (main, LOCAL-ONLY) | commit | main: yes | PASS |

lsp-max note: c7fde33d was consumed by the filter-repo plumbing repair the
seal itself records in its BLOCKED-resolved section (push 5dec93be..c977c1b6
2026-11-04). The merge commit c977c1b6 exists with parents 8371ec54 (origin
lineage) + 5dec93be (repaired lineage). The seal's falsifier as written ("a
commit SHA recorded here does not exist in its repo") technically fires; the
supersession is disclosed inside the same seal, so the standing of this row is
FAIL (receipted supersession — the repaired lineage, not the recorded SHA, is
now the truth on master). Secondary confirmations pass: 5dec93be and c977c1b6
are both ancestors of current master; 7834fd045 (marketplace in-tree ledger
row marker) exists and is ancestral.

## Falsifier class 2 — cited receipts' standing words agree with their bodies (10 spot-checks)

| receipt | closure/cl-close-2 claim | body Standing section (read) | verdict |
|---|---|---|
| ggen receipts/sx-1-reds.md | cl-close-2: "UNSTATED in body (green tables carry the evidence)" | PASS | DRIFT-DISCLOSED (body does state a token — PASS; cl-close-2's "UNSTATED" wording is inaccurate, but no standing contradiction) |
| receipts/af-5-sync-surface.md | ALIVE | ALIVE | PASS |
| receipts/ab-14-chain-scale.md | ALIVE (Status section) | ALIVE | PASS |
| receipts/ga-11-cscb-pack.md | ALIVE | ALIVE | PASS |
| receipts/ga-12-tort-pack.md | ALIVE (tort ceilings TARGET-UNMEASURED) | ALIVE | PASS |
| receipts/af-42-marketplace-fixes.md | ALIVE | ALIVE ("Standing: ALIVE." line 47; the greppable "REFUSED" is a quoted court string, not standing) | PASS |
| receipts/xc-8-cve-bump.md | cl-close-2: "UNSTATED in body (registered divergence carried)" | ALIVE for 16/18; 2 quick-xml rows BLOCKED | DRIFT-DISCLOSED (body states standing; closure's substance — 2 registered unfixable divergences — matches the body's BLOCKED rows exactly) |
| receipts/xc-7-ci-unblock.md | cl-close-2: "UNSTATED in body (post-push CI re-run pending, disclosed)" | "ALIVE locally; CI flip UNVERIFIED" | DRIFT-DISCLOSED (body states standing; substance matches: CI re-run pending) |
| cgap receipts/bk-1-takeover.md | ALIVE | ALIVE | PASS |
| cgap receipts/gd-1-dod-pack.md | ALIVE for pack surface; PARTIAL overall | ALIVE / PARTIAL | PASS |
| arcus receipts/xd-4-govulncheck.md | ALIVE (0-vuln expectation refuted at go1.23.4, clean at go1.26.10) | ALIVE | PASS |

11 spot-checks run (10 contracted + 1 arcus cross-repo). Zero standing
contradictions. Three cl-close-2 index rows mis-characterize a receipt's body
as "UNSTATED in body" when the body states a token — wording drift in the
index row only, substance intact; carried, not repaired (this lane is
create-only on one file).

## Falsifier class 3 — receipt counts: fresh `ls \| wc -l` vs ledger and seal

| repo | fresh (this session) | wave ledger (2026-11-04 freeze) | seal XD-3 refresh | verdict |
|---|---|---|---|---|
| ggen | 110 | 109 | 90 (89 + xd-3) | DRIFT-DISCLOSED (+1 file landed after ledger freeze; seal counts predate the post-seal landings by design) |
| cgap | 66 | 63 | 50 | DRIFT-DISCLOSED (+3 since ledger) |
| arcus | 29 | 29 | 27 | PASS at ledger granularity / DRIFT-DISCLOSED vs seal (+2 over seal refresh) |
| lsp-max | 13 | 13 | (not refreshed) | PASS (exact match) |
| claude-code-config-lsp | 70 | 70 | (not refreshed) | PASS (exact match) |
| ggen-marketplace | 5 | 4 | (not refreshed) | DRIFT-DISCLOSED (+1 since ledger) |

Ledger HEAD-vs-tip drift (disclosed, class 1-adjacent): ggen ledger HEAD
ed3f1dfb19 is an ancestor of current v26-11-3-wave (72b743d23) — fast-forward
drift. cgap/arcus/claude-code-config-lsp HEADs match the ledger exactly
(37721e2f / 1c08fd1 / 6a04124). lsp-max ledger HEAD 7278ae77 does NOT exist —
same filter-repo rewrite that consumed c7fde33d (ledger frozen pre-repair).
ggen-marketplace ledger HEAD 0e94adbb exists and is ancestral of main; main
has since advanced to d451e63e4 and the checkout sits on zd-26-frozen-drift.

RECEIPTS.md status re-check: present in ggen and arcus; absent in cgap,
lsp-max, claude-code-config-lsp, ggen-marketplace — agrees with the ledger's
loud-MISSING record.

## Falsifier class 4 — cl-close-2.md exit-gate rows still resolve (5+ cited receipts exist)

Cited receipts re-checked on disk, all EXIST:

ggen receipts/: sx-1-reds.md, af-5-sync-surface.md, ab-14-chain-scale.md,
ga-11-cscb-pack.md, ga-12-tort-pack.md, af-42-marketplace-fixes.md,
gd-2-ggen-dod.md, xc-8-cve-bump.md, xc-7-ci-unblock.md, xd-2-key-perms.md (10/10 exist, plus xd-3-index-refresh.md).
cgap receipts/: bk-1-takeover.md, af-37-p99-torture.md, rx-2-conformance.md,
gd-1-dod-pack.md, xd-1-deny-toml.md, xd-5-error-taxonomy.md (6/6 exist).
arcus receipts/: xd-4-govercheck → xd-4-govulncheck.md (1/1 exists).

19 cited receipts checked, 19 exist. 9 spot-checks exceed the contracted 5.

## Summary table (per falsifier class per repo)

| repo | class 1 (SHAs) | class 2 (standing) | class 3 (counts) | class 4 (exit-gate cites resolve) |
|---|---|---|---|---|
| ggen | PASS | PASS (7 checked) | DRIFT-DISCLOSED | PASS |
| cgap | PASS | PASS (3 checked) | DRIFT-DISCLOSED | PASS |
| arcus | PASS | PASS (1 checked) | PASS at ledger granularity / DRIFT-DISCLOSED vs seal | PASS |
| lsp-max | FAIL (receipted supersession: filter-repo rewrite) | n/a (none sampled) | PASS | n/a (no cites sampled) |
| claude-code-config-lsp | PASS | n/a | PASS | n/a |
| ggen-marketplace | DRIFT-DISCLOSED (branch move; SHA ancestral) | n/a | DRIFT-DISCLOSED | n/a |

## Standing (this receipt's own)

ALIVE for the contracted verification scope: all four falsifier classes were
run against real git/file state this session with the commands named above;
zero edits outside this file; zero git state changes. Zero standing-word
contradictions found across 11 spot-checked receipts. One falsifier fires
(lsp-max c7fde33d non-existence) with the supersession receipted inside the
seal itself; the repaired lineage is ancestor-verified on current master. All
count deltas are disclosed-late-landing drift, none silent.

## Falsifiers (this receipt's own)

- A re-run of any command above returning a different verdict falsifies this
  receipt.
- A receipt body contradicting the cl-close-2 row's standing word (beyond the
  three "UNSTATED" wording drifts recorded in class 2) falsifies this
  receipt.
- The receipt count for any repo changing under a fresh `ls \| wc -l` between
  this receipt's write and any future cite re-reads as drift, per the
  seal's own law.
