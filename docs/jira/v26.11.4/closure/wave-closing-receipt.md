# Wave-Closing Receipt — v26.11.4

as-of <fill timestamp> — every count read live at fill.

Standing: DRAFT-UNTIL-DRAIN. Sections are skeleton-only; each carries a
FILL comment naming the live command whose output belongs there. Nothing
in this file is a claim until the section is filled.

## <!-- FILL: subject identity -->

<!-- FILL: `git rev-parse HEAD` + `git branch --show-current` + dirty
     state, per repo (ggen/arcus/cgap/lsp-max/marketplace/smalt). -->

## <!-- FILL: receipt census -->

<!-- FILL: `ls docs/jira/v26.11.4/receipts/ | wc -l`, per repo, plus the
     per-receipt standing word re-read from each body (never from
     memory). -->

## <!-- FILL: verification ladder -->

<!-- FILL: per-rung commands + exit records, rung-1..8 (narrow -> unit ->
     integration -> e2e -> chaos -> stress -> benchmark -> machine
     report); rung-7 stress/chaos stays BLOCKED per open-items OI-4
     unless zd-79 lands. -->

## <!-- FILL: gate results -->

<!-- FILL: fmt/clippy/test/gate script names + exit codes run live at
     fill time. -->

## <!-- FILL: corrections ledger -->

<!-- FILL: any row corrected vs its prior claim (first entry: zp-16
     receipt BLOCKED claim stale — engine pins now =26.11.4; see
     open-items-register.md OI-1). -->

## <!-- FILL: standing summary -->

<!-- FILL: counts per standing word (ALIVE/PARTIAL/BLOCKED/OPEN/PLANNED/
     UNVERIFIED) re-read from the register + receipts at fill. -->

## <!-- FILL: replay -->

<!-- FILL: `bash docs/jira/v26.11.4/closure/replay-commands.sh` output
     appended at fill, exit 0 required. -->
