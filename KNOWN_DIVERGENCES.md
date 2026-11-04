# KNOWN_DIVERGENCES

Registered divergences for ggen, seeded by transcription from existing
receipts/RESOLUTIONS entries. Every row carries a surface, a divergence, an
evidence path, and a close condition. A row with no evidence path is
invalid; a closed row without a post-close receipt is invalid.

Status vocabulary: `OPEN` (unresolved, owner named), `PARTIAL`,
`CLOSED` (witnessed resolved), `CLOSED (upstream)` (resolved only by an
upstream release).

| id | status | surface |
|---|---|---|
| XC-8/ZD-38 | OPEN (upstream) | quick-xml 0.37.5 CVE pair via oxigraph family |
| R13 | OPEN | `--skip-ea-admission` opt-out flag (registered divergence, never default-on) |
| ZD-135 | PARTIAL | tcps-generated FM-PACK-008 (52/53 validator checks) |

## XC-8/ZD-38 — quick-xml CVE pair, upstream-blocked

**Status:** OPEN (upstream).

`cargo update -p quick-xml@0.37.5 --precise 0.41.0` fails:
`process_mining v0.6.2` (latest 0.6.x) requires `quick-xml ^0.37.4`. The
0.37.5 copy enters only via the oxigraph family (oxrdfxml 0.2.4,
sparesults 0.3.4, process_mining 0.6.2) — no workspace code imports
`quick_xml` directly. Closing these CVEs requires a process_mining
release on quick-xml 0.38+; unfixable by workspace version bump.

**Evidence:** `docs/jira/v26.11.3/receipts/xc-8-cve-bump.md:35`.

**Close:** process_mining publishes a release requiring quick-xml
0.38+; workspace re-attempts the precise bump and re-runs the vuln
gate. Flip CLOSED (upstream) with the new lock diff + gate exit.

## R13 — `--skip-ea-admission` opt-out, never a default-on bypass

**Status:** OPEN.

The EA-graph Stage 2c opt-out is the named CLI flag
`--skip-ea-admission`; a warning-and-continue is a fail-open defect
(coding-agent-mistakes class 3). If used, the flag is named in the
receipt and registers as this registered divergence — never a default-on
bypass. The missing-graph fail-closed gate and this flag apply
identically on both paths; an owned court covers it.

**Evidence:** `docs/jira/v26.11.3/RESOLUTIONS.md:72` (R13; instance of
R3 at `:26`).

**Close:** the opt-out is removed once EA-graph admission is
hermetic in every environment (no environment can fail Stage 2c
legitimately); until then every use must be receipt-named. Flip CLOSED
with the flag-removal diff + court.

## ZD-135 — tcps-generated FM-PACK-008

**Status:** PARTIAL.

Check 1 of the committed-confirm ladder: 52/53 checks pass, one
registered divergence (tcps-generated FM-PACK-008), plus a
binary-dependency caveat (the line-145 forbidden-surface gate
`rg`-scan silently skipped locally — no `rg` binary on the bash PATH;
CI ships rg, so that sub-check stands UNVERIFIED locally).

**Evidence:** `docs/jira/v26.11.3/receipts/zd-135-committed-confirm.md:30`.

**Close:** tcps-generated FM-PACK-008 resolves (pack fixed or
gate re-scoped) and the ladder re-runs 53/53 with a real `rg`-witnessed
exit on this host. Flip CLOSED with the receipt.
