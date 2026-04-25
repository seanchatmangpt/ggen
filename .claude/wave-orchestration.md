# ggen v6.1.0 Wave Orchestration

**Goal:** Publish v6.1.0 to crates.io AND validate CLI via `cargo install ggen` inside a testcontainer.
**Stop condition:** Either 10 waves complete OR both goals achieved (published + container-validated).

## Wave Status

| Wave | Tasks | Status | Exit Criteria |
|------|-------|--------|---------------|
| 1 | Compile fix, registry.rs, marketplace tests, receipt/core fix, OTEL/publish checklist | 🔄 RUNNING | `cargo make check` passes |
| 2 | Remaining errors, full test suite, doctor command, capability fix, binary rebuild | ⏳ PENDING | `cargo make test` passes + `ggen --version` = 6.1.0 |
| 3 | Lint clean, CLI completeness, golden path | ⏳ PENDING | `cargo make lint` clean + golden path runs |
| 4 | Full pre-commit, integration tests, OTEL spans | ⏳ PENDING | `cargo make pre-commit` passes + OTEL verified |
| 5 | Publish foundation crates (phase 1-3) | ⏳ PENDING | ggen-utils, ggen-core etc at v6.1.0 on crates.io |
| 6 | Publish middle-layer crates (phase 4-7) | ⏳ PENDING | domain, marketplace, CLI crates published |
| 7 | Publish ggen binary + verify crates.io | ⏳ PENDING | `cargo install ggen` works from crates.io |
| 8 | Testcontainer install + golden path | ⏳ PENDING | Container: install + capability→pack→sync→receipt |
| 9 | Container CISO + OTEL validation | ⏳ PENDING | Container: enterprise-strict + weaver live-check |
| 10 | Final validation + completion report | ⏳ PENDING | All evidence collected, report written |

## Wave 1 Agents (RUNNING)

| Agent ID | Task |
|----------|------|
| a21b2a66b1c5867cf | Fix ALL compilation errors |
| ad55b2038658f2538 | Create missing registry.rs |
| a6a5d8dd75f140b98 | Fix marketplace test failures |
| aab7c8291551cb8a2 | Fix receipt + core crates |
| ac91f6c6d6b8316b1 | Golden path + OTEL weaver + publish checklist |

## Blockers Identified (from Ralph Loop iteration 1)

1. **Compilation:** 93+ errors — partially fixed in commit `fix(v6.1.0)` by Agent 10
2. **CLI:** `registry.rs` missing, `doctor` unimplemented, `capability` not auto-discovered
3. **Marketplace tests:** `regulated_finance_profile` and other missing API imports
4. **Version:** Binary shows v5.5.0; code says v6.1.0 — needs rebuild
5. **Publish deps:** `ggen-a2a-mcp` and other crates not yet published to crates.io

## Golden Path (must work in testcontainer by Wave 8)

```bash
ggen capability enable compliance.soc2
ggen pack add mcp-rust
ggen sync --profile enterprise-strict --locked
ggen receipt verify ./sync-receipt.json
```

## Publish Order (26 crates, 13 phases from Agent 6 report)

Phase 1: ggen-utils
Phase 2: ggen-config, ggen-canonical, ggen-receipt
Phase 3: ggen-core, ggen-codegen, ggen-ontology-core
Phase 4: ggen-domain, ggen-ai, ggen-prompt-mfg
Phase 5: ggen-marketplace, ggen-a2a, ggen-transport
Phase 6: ggen-a2a-registry, ggen-a2a-mcp, a2a-generated
Phase 7: ggen-cli-validation, ggen-config-clap
Phase 8: ggen-dspy, ggen-yawl
Phase 9: ggen-process-mining
Phase 10: ggen-node (skip if napi issues)
Phase 11: ggen-cli
Phase 12: ggen (binary)
Phase 13: Verify all on crates.io

## Completion Evidence Required

- [ ] `cargo install ggen` from crates.io succeeds
- [ ] `ggen --version` in container = 6.1.0
- [ ] `ggen capability enable compliance.soc2` works
- [ ] `ggen pack add mcp-rust` works
- [ ] `ggen sync --profile enterprise-strict --locked` works
- [ ] `ggen receipt verify` works
- [ ] OTEL weaver live-check: all spans pass
- [ ] CISO enterprise-strict: fail-closed enforcement confirmed
