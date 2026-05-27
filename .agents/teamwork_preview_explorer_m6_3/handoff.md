# Handoff Report — Contradiction Detection & Adjudication Design

This report handsoff the design and logic formulation for `12_detect_contradictions.sh` and `13_adjudicate_gall_promotion.sh` under the External Observer Script Ring.

## 1. Observation
- The standalone RDF crate `crates/ggen-graph` contains `verify_audit.rs` (lines 1–135) to enforce 5 completeness rules on the self-audit log and coverage matrix.
- We analyzed `crates/ggen-graph/audit/vision2030.self_audit.ocel.json` (lines 264–297) and observed:
  1. Checkpoint `obj_cp_verify` has both a `CheckpointPromoted` event and a `CheckpointRefused` event, representing a direct decision contradiction.
  2. The event log has a test failure event `ev_test_failed` at `09:35:00Z` and a subsequent checkpoint promotion event `ev_checkpoint_promoted` at `10:10:00Z` without any intervening successful `TestPassed` event.
- We verified that the tools `jq` (version 1.7.1) and `b3sum` (version 1.8.5) are installed and available in the shell environment.

## 2. Logic Chain
- To detect conflicting checkpoints and missing evaluations, `12_detect_contradictions.sh` must scan the OCEL event log using deterministic JQ filters. The logic checks for:
  - Double Decisions: Promoted AND Refused checkpoints.
  - Evaluation Gaps: Adjudications without preceding evaluations.
  - Un-adjudicated Checkpoints: Evaluations without subsequent decisions.
  - Undeclared/Un-evaluated Checkpoints: Top-level declared checkpoint objects with no evaluation events.
  - Failed State Promotions: Promotion events happening with preceding un-remediated test failures.
  - Adjudicator ID reuse: PromotionDecision IDs mapped to multiple checkpoints.
- To prevent bypass and mutation, `13_adjudicate_gall_promotion.sh` must verify script/source file integrity against a checksum manifest `manifest.sha256`, execute the verification scripts (00 to 12) sequentially, run the contradiction detector, and compute a cryptographic BLAKE3 receipt over the resulting JSON metadata before writing to `crates/ggen-graph/audit/vision2030.external_adjudication.json`.
- The final proof document `docs/VISION_2030_GALL_PROOF.md` must declare strict dependency on the external observer script ring's output (`vision2030.external_adjudication.json`) instead of self-verifying.

## 3. Caveats
- **Manifest Setup**: The hashes in `manifest.sha256` are not pre-defined since the scripts under `scripts/gall/external/` (00 to 12) are yet to be written. The implementation agent must generate these hashes once the script ring is finalized.
- **Environment Dependencies**: The shell script solutions rely on `jq` and `b3sum` commands being available in the path.

## 4. Conclusion
- The designed shell scripts for contradiction detection and promotion adjudication are structurally and logically complete. They enforce anti-tampering and logic consistency gates that strictly satisfy the Chicago TDD and anti-cheating guidelines (no placeholders or stubs).
- The current self-audit JSON log is contradictory, and the self-audit log generator in `self_audit.rs` must be corrected by the implementation team to ensure the scripts exit successfully.

## 5. Verification Method
- Execute the JQ checks designed in `12_detect_contradictions.sh` directly on `crates/ggen-graph/audit/vision2030.self_audit.ocel.json` to verify that they correctly identify the contradictions.
- Copy the code of `12_detect_contradictions.sh` and `13_adjudicate_gall_promotion.sh` to their respective paths, configure the manifest, and run them sequentially under zsh on macOS.
- Check that the resulting output file `crates/ggen-graph/audit/vision2030.external_adjudication.json` conforms to the proposed schema and contains a valid BLAKE3 receipt hash.
