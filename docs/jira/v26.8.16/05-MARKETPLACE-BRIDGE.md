# 05 — Marketplace Bridge: Two Implementations That Never Meet

Part of [00-OVERVIEW](00-OVERVIEW.md). Decision ticket first, wiring ticket second —
closing it with a written non-goal is a valid outcome.

## Finding (survey, 2026-08-16)

Two independent marketplace implementations exist with no connection:

1. **ggen side — ALIVE.** `/Users/sac/ggen/target/release/ggen pack list --format plain`
   exits 0 and returns 2 real packs: `framework-lsp` (v26.6.8, 3 packages / 3 templates,
   registry_type: local) and `tower-lsp-max` (v26.6.8, 7 packages / 4 templates).
   Subcommands present: `pack search/list/new/...`. The new
   `packs/goat-capabilities-pack/` ([01](01-COMMIT-BOUNDARY.md)) makes 3.
2. **ostar side — module exists, unconnected.** `/Users/sac/chatmangpt/ostar/src/ostar/
   marketplace/` (`entitlement.py`, `gate.py`, `example.py`, receipts wiring into
   `ostar.manufacturing.prov_receipt`) imports clean after the sweep, but nothing in it
   invokes or is invoked by ggen's pack registry; the only ostar→ggen call paths are
   `bridge.py` (MuStar plan) and the E2E proof's cargo phases.

The Chatman Equation pipeline (ostar plans → ggen generates → receipts prove) implies the
ostar marketplace's entitlement/gate layer was meant to govern ggen pack
installs/publishes — but that edge was never built, and no doc records deciding against
it.

## Fix — decide, then either wire or write it down

**Step 1 (required): decision.** Is ostar's marketplace layer supposed to govern ggen
packs? Owner: Sean. Record the answer in this file.

**Step 2a (if yes): smallest real edge.** One honest call path, not a framework:
ostar-side function that shells to `ggen pack list --format json` (mirroring `bridge.py`'s
subprocess pattern), feeds each pack through `ostar.marketplace.gate`, and returns a real
admit/refuse decision with a receipt. Chicago-verified end to end: real binary, real
packs, assert on the real decision object — and covered by the re-enabled suite from
[04](04-MARKETPLACE-TEST-SUITE-DISABLED.md) on the ggen side.

**Step 2b (if no): non-goal note.** Add a short section to
`/Users/sac/chatmangpt/ostar/src/ostar/marketplace/__init__.py`'s module docstring and to
this ticket stating the two marketplaces are deliberately independent and what each
governs, so the next sweep does not re-flag this as half-wired.

## Acceptance

- Decision recorded here with a date.
- If wired: real command + real output showing a pack admitted/refused through the ostar
  gate, plus the receipt artifact path.
- If non-goal: docstring updated; this ticket closed as WONTFIX-by-design.

## See Also

- [04-MARKETPLACE-TEST-SUITE-DISABLED](04-MARKETPLACE-TEST-SUITE-DISABLED.md)
- `/Users/sac/chatmangpt/ostar/src/ostar/bridge.py` — the existing subprocess bridge
  pattern step 2a should mirror
