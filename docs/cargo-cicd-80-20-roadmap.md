# cargo-cicd 80/20 Roadmap

20% of additions that deliver 80% of the remaining value, ordered by impact.

---

## 1. `cargo cicd affidavit seal` as a first-class CI gate

**What**: After `pipeline run`, seal the XES evidence into a BLAKE3 receipt via `cargo cicd affidavit seal`, then `cargo cicd affidavit verify` in a downstream job to certify the trace is untampered.

**Why this is the highest-leverage addition**: The autonomic evidence pipeline (events.jsonl → events.xes → audit-events.xes) already runs. The BLAKE3 seal converts that output from "present" to "cryptographically bound." Without it the evidence log can be regenerated or overwritten silently; with it any modification breaks verification. This is the last step to make the provenance chain unforgeable — consistent with ggen's own `.ggen/receipts/` pattern and the `coding-agent-mistakes.md` Contract Drift invariant.

**Implementation sketch for ci.yml**:
```yaml
- name: Seal evidence receipt
  run: cargo cicd affidavit seal

- name: Verify sealed receipt
  run: cargo cicd affidavit verify
```

---

## 2. `cargo cicd lsp check` on changed `.rs` files

**What**: Run `cargo cicd lsp check` (requires `anti-llm-cheat` feature) against the diff on every PR. It scans changed Rust files for anti-LLM admissibility violations — fabricated examples, undeclared dependencies, stub implementations.

**Why this is high leverage**: ggen's `CLAUDE.md` and `testing-anti-cheating.md` require zero fabrication, Chicago TDD only, and real-collaborator evidence. Currently those rules are enforced by convention. `lsp check` makes them structurally enforced on every PR, converting a human-review requirement into a CI gate. The `anti-llm-cheat` feature is already compiled into cargo-cicd; it just needs wiring.

**Implementation sketch**:
```yaml
# In the plugin-harness or a new lsp-check job:
- name: Anti-LLM admissibility check
  run: cargo cicd lsp check
```

Add `--features anti-llm-cheat` to the install step in `install-cargo-tools/action.yml`.

---

## 3. `cargo cicd workspace sync` after build in `comprehensive-test`

**What**: After the `comprehensive-test` job builds the workspace, call `cargo cicd workspace sync` which invokes `ggen sync` if `ggen.toml` is present. This proves the RDF → code pipeline still executes cleanly on every CI run.

**Why this is high leverage**: ggen's definition of done requires `just sync` to pass. Currently CI never exercises the full μ₁–μ₅ pipeline. The `workspace sync` verb degrades gracefully (skips and passes if `ggen` binary not found), so adding it is low risk. When `ggen` IS on PATH (after the comprehensive-test build step), it gives a real pipeline smoke test on every PR.

**Implementation sketch** (add after the last test step in `comprehensive-test`):
```yaml
- name: ggen pipeline smoke (workspace sync)
  run: |
    cargo build -p ggen-cli --bin ggen
    PATH="$PWD/target/debug:$PATH" cargo cicd workspace sync
```

---

## 4. `cargo cicd status audit` as a release gate

**What**: In `release.yml`, after `workspace doctor`, run `cargo cicd status audit`. This shells to the `wpm` oracle on `events.xes` and returns ACCEPT or REFUSE. Only on ACCEPT does the release proceed.

**Why this is high leverage**: The release job currently gates only on `workspace doctor` (structural health). `status audit` adds a process conformance gate — the release only proceeds if the evidence log shows a lawful sequence of events (fitness ≥ 0.95 threshold). This closes the loop between evidence emission (already wired) and release authorization. It's the `publish_not_adjudicated` autonomic policy made mandatory.

**Prerequisite**: `wpm` binary must be present in CI. Install via:
```yaml
- name: Install wpm oracle
  run: cargo install --git https://github.com/seanchatmangpt/wasm4pm --bin wpm --locked
```

---

## 5. IEC 61508 / ISO 26262 compliance summary as CI artifact

**What**: cargo-cicd's `certification` module already maps each command (`status show`, `workspace doctor`, `test changed`, `pipeline run`, `evidence audit`, `publish run`) to IEC 61508 clause numbers and ISO 26262 ASIL requirements. Expose this as a CI artifact by calling a `cargo cicd certification show` command (add this verb to the `certification` noun) and uploading the output.

**Why this is worth adding**: ggen generates Rust code that may be used in safety-critical contexts. A per-release compliance summary — showing which IEC 61508 / ISO 26262 clauses are evidenced by the pipeline — makes the tool credible for regulated environments. The evidence mappings are already in the codebase (`iec_61508.rs`, `iso_26262.rs`); they just need a `show` verb that prints the summary and exits 0.

**What needs to be added to cargo-cicd itself**: A `CertificationNoun` with a `show` verb that calls `iec_61508::compliance_summary()` and `iso_26262::compliance_summary()` and prints them.

---

## What NOT to add (the other 80%)

| Feature | Why to skip |
|---|---|
| `cargo cicd ui dashboard` in CI | Terminal UI is for humans; structured output is better for CI logs |
| `cargo cicd autoarch tune` in CI | UCB1 bandit threshold tuning is a local iteration tool, not a CI gate |
| `cargo cicd analyze dep-order` | Useful for debugging build order; not a quality gate |
| `cargo cicd git commit` / `git stage` | Agents committing in CI without human review is risky |
| `cargo cicd trybuild update` in CI | Snapshot updates should be explicit, not automatic |
