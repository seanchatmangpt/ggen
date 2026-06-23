# SWARM ADAPTATION PLAN: Speckit-Ralph to Genesis Sabotage Corpus

## 1. Executive Summary
This document defines the strategy for transitioning `speckit-ralph` from an exploratory toolset into a formal Genesis Sabotage Corpus. This transformation ensures that all existing exploit and sabotage surface mappings adhere to the M2M Closure Protocol, enabling deterministic validation of system integrity and refusal proofs.

## 2. Transition Objectives
- Formalize "exploratory" scripts into deterministic Construct8 test triggers.
- Align all test evidence with M2M Closure Protocol mandates.
- Establish a rigorous boundary-crossing validation suite for refusal surface proofs.

## 3. Adaptation Roadmap

### Phase A: Evidence Normalization
- Audit current `test-evidence/` and `receipts/`.
- Migrate all "placeholder" evidence to cryptographically bound raw boundary data (OTel, OCEL logs).
- Replace non-deterministic exploratory scripts with Construct8-compliant triggers.

### Phase B: Construct8 Test Integration
- Convert existing `scripts/*.sh` and `adapters/*.sh` exploit surfaces into `src/` modules in `sos-truthforge` or `sos-powl8`.
- Implement `Construct8` compliant triggers that require real system transitions.
- Validate every transition against the OCEL invariant framework defined in `ostar`.

### Phase C: M2M Closure Protocol Compliance
- Enforce the "Anti-Cheating" principle: faking the sabotage proof must be harder than generating it through real system boundary crossing.
- Integrate mandatory OTel/OCEL validation gates for every trigger execution.
- Update `GAP-CLOSURE-PLAN.md` to track residual exploratory surfaces.

## 4. Implementation Steps

1. **Surface Audit**: Use `scripts/allowlist-scan.py` to identify remaining exploratory scripts that lack M2M receipts.
2. **Canonical Proof Conversion**: Rewrite identified scripts into `sos-truthforge` tests, ensuring they utilize `ostar.process.mining`.
3. **Receipt Binding**: Update the `Construct8` triggers to emit BLAKE3 receipts over the raw OTel span body and OCEL event transition.
4. **Validation Hardening**: Ensure `scripts/validate-001-gate.sh` and similar checks pass with the new triggers.

## 5. Success Metrics
- 100% of exploit surfaces are covered by `Construct8` tests.
- Zero "placeholder" evidence in any new test receipts.
- All triggers produce externally verifiable evidence (OTel/OCEL).
- Full compliance with `OSTAR_TESTING_DOCTRINE`.
