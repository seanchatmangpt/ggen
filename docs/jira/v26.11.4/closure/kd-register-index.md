# KD Register Index — pointer rows only

as-of 2026-11-04T21:41Z. This file is an index, not a register: each row
points at the owning repo's register. Lines <=100 chars.

| register | path | claim (one line) | falsifier |
|---|---|---|---|
| cgap KD-1..7 | /Users/sac/cgap/docs/jira/v26.11.4/KNOWN_DIVERGENCES.md | KD-1 version-refusal status-word collision (OE-4); KD-2 benchmark ns/frame claims do not bound contention; KD-3..7 v26.11.1 campaign ledger migrated verbatim | any rowed KD closes without a witnessed recovery receipt |
| arcus KD-1..3 | /Users/sac/arcus/KNOWN_DIVERGENCES.md | KD-1 cgap_import_arrow gap strategy (b); KD-2 replay --dump-arrow seq/schema-identity form; KD-3 hermetic lint pin (golangci via tools/go.mod) | arcus adopts the diverged-upstream form without a register row |
| ggen XC-8/R13/ZD-135 | /Users/sac/ggen/KNOWN_DIVERGENCES.md | XC-8/ZD-38 quick-xml 0.37.5 CVE pair upstream-blocked; R13 --skip-ea-admission opt-out never default-on; ZD-135 tcps FM-PACK-008 52/53 checks | a divergence closes silently (no row update) or the opt-out ships default-on |
| smalt posh KD | /Users/sac/smalt/posh/KNOWN_DIVERGENCES.md | coverage cells must be PASS / registered KD-nn row / loudly MISSING; KD-06 `* (all cells)` prose matched 0 cells while silently skipped (previously silent) | a gate accepts a cell that is neither pass, rowed, nor loudly missing |
