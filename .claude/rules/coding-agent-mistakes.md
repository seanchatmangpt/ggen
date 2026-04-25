---
version: 6.1.0
last_updated: 2026-04-02
gate: mandatory — read before every agent dispatch
---

# Coding-Agent Mistakes — Mandatory Gate

> **The strongest single rule:** Every coding-agent patch must either deepen authority or reduce drift.
>
> This means: the patch must make the authoritative path harder to bypass, make bypasses fail loudly, or remove a bypass that already exists. A patch that adds a feature while leaving the old bypass intact does not satisfy this rule.

---

## 1. The Five Mistake Classes

### 1.1 Decorative Completion

**Definition:** A command exits 0 and prints success, but no durable state transition occurred. The world looks the same after the operation as before.

**ggen examples:**
- `ggen packs add acme/base` logs "Pack added" but `.ggen/packs.lock` is not written or is unchanged.
- `ggen sync` emits "Sync complete" but `.ggen/receipts/` contains no new file and `.ggen/packs.lock` timestamp is unchanged.
- A receipt is created with an empty `signature` field (`""`).

**Detection:**
```bash
# Before/after diff on lockfile mtime
stat -f "%m" .ggen/packs.lock   # macOS
stat -c "%Y" .ggen/packs.lock   # Linux

# Receipt field check — signature must be non-empty
jq -e '.signature | length > 0' .ggen/receipts/latest.json

# Sabotage: if lockfile absent, next sync --locked must hard-fail
rm .ggen/packs.lock && ggen sync --locked   # must exit non-zero
```

---

### 1.2 Epistemic Bypass

**Definition:** Logic that should be derived from the RDF ontology, a SPARQL query, or a Tera template is instead hardcoded inline. The codebase "knows" something it should only "ask."

**ggen examples:**
- A CLI handler matches pack names with a `match` arm instead of querying the `PackRegistry`.
- A validation rule embeds SHACL shapes as string literals rather than loading them from `.specify/shapes/`.
- A sync step hardcodes the list of output file paths rather than deriving them from a `CONSTRUCT` query over the ontology.

**Detection:**
```bash
# Find hardcoded pack names outside registry layer
cargo make lint  # clippy::match_wildcard_for_single_variants, manual string matching

# Grep for inline TTL/shapes strings (should never appear outside test fixtures)
grep -rn 'sh:NodeShape\|sh:property' crates/*/src/ --include='*.rs'

# Find hardcoded output paths that bypass template resolution
grep -rn '"crates/' crates/ggen-core/src/ --include='*.rs'
```

---

### 1.3 Fail-Open Behavior

**Definition:** The system continues executing when it should halt. A missing required resource produces a warning instead of an error; a violated constraint is logged but not enforced.

**ggen examples:**
- A missing pack at `ggen sync` time emits `warn!("pack not found, skipping")` instead of returning `Err(...)`.
- A profile violation (strict mode) appends a warning to the receipt but still writes output artifacts.
- `ggen receipt verify` returns `is_valid: true` when the signature field is empty.

**Detection:**
```bash
# Verify that missing-pack triggers an error exit code, not a warning
GGEN_PACKS_DIR=/tmp/empty-packs-dir ggen packs add no-such-pack
echo "exit: $?"  # must be non-zero

# Verify that --locked fails hard when lockfile is absent
rm -f .ggen/packs.lock
ggen sync --locked
echo "exit: $?"  # must be non-zero

# Verify strict-profile violation blocks artifact emission
# (profile enforcement test lives in crates/ggen-marketplace/tests/profile_enforcement_test.rs)
cargo make test -- profile_enforcement
```

---

### 1.4 Legacy Path Contamination

**Definition:** A new authoritative path was built correctly, but the old bypass was not removed. Both paths coexist. The old path, being simpler, is hit in practice.

**ggen examples:**
- `ggen sync` was refactored to consume `.ggen/packs.lock`, but a fallback `load_packs_legacy()` branch still executes when the lockfile is absent (fail-open + legacy contamination together).
- `ggen init` writes `ggen.toml` via a new `ConfigWriter` but also retains a `write_default_config()` function that writes a different, incomplete schema.
- A new `ReceiptManager` was introduced but `write_raw_receipt()` still exists and is called from three non-test sites.

**Detection:**
```bash
# Find legacy function names still in non-test code
grep -rn 'load_packs_legacy\|write_raw_receipt\|write_default_config' \
  crates/*/src/ --include='*.rs'

# Confirm new path is the only path (no dead fallback)
cargo make check 2>&1 | grep 'dead_code'

# Inspect call sites for dual-path routing
# Use LSP findReferences on the old function to enumerate callers
```

---

### 1.5 Contract Drift

**Definition:** The receipt, lockfile, or other proof object no longer accurately describes what actually ran. Fields are stale, absent, or populated with defaults that were never replaced.

**ggen examples:**
- `.ggen/packs.lock` contains `"digest": ""` because the pack TOML was not hashed at install time.
- A receipt records `input_hashes` from the previous sync run because the hash step was skipped on incremental re-run.
- `operation_id` in a receipt is a hardcoded test UUID that was never replaced with a real `Uuid::new_v4()`.

**Detection:**
```bash
# Check for empty or default-sentinel values in lockfile
jq '.packages[] | select(.digest == "" or .digest == null)' .ggen/packs.lock

# Check for hardcoded UUIDs in receipts
grep -rn '00000000-0000-0000-0000-000000000000' .ggen/receipts/

# Verify receipt reflects current run's input hashes
# (run sync twice with different inputs; receipts must differ)
ggen sync && cp .ggen/receipts/latest.json /tmp/r1.json
touch some_input_file
ggen sync && diff /tmp/r1.json .ggen/receipts/latest.json  # must differ
```

---

## 2. The Authoritative Path for ggen v6.1.0

```
intent
  → capability/pack resolution      (PackRegistry, .ggen/packs.lock)
  → lock / trust / profile check    (--locked flag, profile constraints)
  → sync (consuming packs)          (μ₁–μ₅ pipeline over locked pack set)
  → validation                      (SHACL gates, quality gates)
  → render                          (Tera templates from ontology-derived bindings)
  → receipt (provenance)            (.ggen/receipts/*.json, Ed25519 signature)
```

### What "touches" each stage

| Stage | Authoritative implementation | File/module |
|-------|------------------------------|-------------|
| Pack resolution | `PackRegistry::resolve()` reading `.ggen/packs.lock` | `crates/ggen-marketplace/src/` |
| Lock/trust check | `SyncCommand` validates lockfile before any I/O | `crates/ggen-cli/src/cmds/sync.rs` |
| Profile check | `ProfileEnforcer` blocks emit on strict violation | `crates/ggen-marketplace/src/rdf/control.rs` |
| μ₁–μ₅ pipeline | `Pipeline::run()` in `ggen-core` | `crates/ggen-core/src/` |
| SHACL validation | `SHACLGate::check()` | `crates/ggen-core/src/validation/` |
| Render | `TemplateResolver::render()` via Tera | `crates/ggen-core/src/` |
| Receipt | `ReceiptManager::emit()` signing with Ed25519 | `crates/ggen-receipt/src/` |

### What "bypasses" each stage

| Bypass | Why it is forbidden |
|--------|---------------------|
| Running sync without reading lockfile | Decorative completion — pack set is undefined |
| Emitting artifacts before profile check | Fail-open — violating profile constraints silently |
| Writing receipt without real signature | Contract drift — proof object is meaningless |
| Skipping μ₁ (load) and hardcoding graph | Epistemic bypass — ontology is not the source of truth |
| Leaving `load_packs_legacy()` reachable | Legacy path contamination |

---

## 3. The 6-Question Patch Contract

Every agent patch must answer all six questions before the patch is accepted.

### Q1: What real state changed?

Not stdout. Name the file, database row, or in-memory structure that is different after this patch runs successfully.

> "`.ggen/packs.lock` gains a new entry with non-empty `digest` and `installed_at`."

### Q2: What authoritative path did this patch touch?

Name the stage from Section 2.

> "Pack resolution stage — `PackRegistry::resolve()` now writes the lockfile atomically."

### Q3: What negative path now fails correctly?

Describe the sabotage condition and the expected error.

> "If the pack TOML is deleted after install, `ggen sync --locked` exits non-zero with `Error: pack digest mismatch`."

### Q4: What invariant protects this patch from drift?

Reference an invariant from Section 4.

> "Lockfile invariant: `digest` must be a non-empty SHA-256. The serialization step enforces `!digest.is_empty()` or returns `Err`."

### Q5: What legacy path was removed or blocked?

If no legacy path was removed, explain why none exists. Silence on this question is a red flag.

> "`load_packs_legacy()` was deleted in this patch. Its three call sites were updated to use `PackRegistry::resolve()`."

### Q6: What proof object shows it worked?

Reference the receipt, test output, or OTEL span.

> "`.ggen/receipts/latest.json` has non-empty `signature` and `input_hashes` includes the pack digest. `cargo make test -- pack_install_lockfile` passes."

---

## 4. Invariant Definitions

### 4.1 Lockfile invariants

Every entry in `.ggen/packs.lock` must satisfy:

```json
{
  "id":           "<string: non-empty>",
  "version":      "<string: semver>",
  "installed_at": "<string: RFC-3339 timestamp>",
  "packages":     ["<string: package path>"],
  "digest":       "<string: SHA-256 hex, 64 chars>"
}
```

- `digest` must be the SHA-256 of the pack TOML at install time.
- `digest` must be re-verified at `ggen sync --locked` time; mismatch must hard-fail.
- `installed_at` must be a real timestamp, never a zero value or empty string.

### 4.2 Receipt invariants

Every `.ggen/receipts/*.json` must satisfy:

```json
{
  "operation_id":   "<string: UUID v4, non-zero>",
  "timestamp":      "<string: RFC-3339>",
  "input_hashes":   { "<pack@version>": "<sha256 hex>" },
  "output_hashes":  { "<relative path>": "<sha256 hex>" },
  "signature":      "<string: base64 Ed25519, non-empty>"
}
```

- `input_hashes` must include every pack consumed during the sync run at its locked version.
- `output_hashes` must include every artifact written during render.
- `signature` must be produced by the private key in `.ggen/keys/signing.key`.
- An empty `signature` field means the receipt is invalid — `ggen receipt verify` must return `is_valid: false`.

### 4.3 Sync invariants

1. `ggen sync` must read `.ggen/packs.lock` before executing any pipeline stage.
2. `ggen sync` must emit a receipt after the render stage completes successfully.
3. `ggen sync --locked` must fail hard (`exit 1`) if the lockfile is absent or its digests do not match installed packs.
4. `ggen sync` must not write any output artifact if a strict-profile validation gate fires.
5. The receipt emitted by sync must reflect the pack set and input hashes of the current run, not a prior run.

---

## 5. Sabotage Test Requirements

These tests must exist and pass. Each is a negative-path test proving the system fails loudly rather than silently.

| Sabotage | Command | Required outcome |
|----------|---------|-----------------|
| Remove pack TOML after install | `rm $GGEN_PACKS_DIR/acme/base.toml && ggen sync --locked` | Exit non-zero; error message references digest mismatch or missing pack |
| Corrupt `.ggen/packs.lock` | `echo 'garbage' > .ggen/packs.lock && ggen sync --locked` | Exit non-zero; error message references invalid lockfile |
| Corrupt latest receipt | `echo '{}' > .ggen/receipts/latest.json && ggen receipt verify` | `is_valid: false` in output |
| Delete verifying key | `rm .ggen/keys/verifying.key && ggen receipt verify` | `is_valid: false` or error; must not return `is_valid: true` |
| Empty packs dir | `GGEN_PACKS_DIR=/tmp/empty && ggen packs add acme/base` | Exit non-zero; error message references "pack not found" |

The test file for profile enforcement is `crates/ggen-marketplace/tests/profile_enforcement_test.rs`. Sabotage tests for sync and receipts belong in `crates/ggen-cli/tests/` or the relevant integration test directory.

---

## 6. The Strongest Single Rule

> **Every coding-agent patch must either deepen authority or reduce drift.**

### What this means for ggen specifically

**Deepening authority** means the authoritative path (Section 2) becomes harder to bypass:
- A new `#[must_use]` return value forces callers to handle the receipt.
- A new typestate prevents calling `render()` before `validate()` compiles.
- A new `--locked` flag makes the fast path the safe path.
- A SPARQL query replaces an inline `Vec` of hardcoded values.

**Reducing drift** means proof objects more accurately reflect what ran:
- `digest` is now computed at install time instead of defaulting to `""`.
- `input_hashes` now include pack versions instead of just pack names.
- `signature` is now verified against the receipt body instead of trusted blindly.

**A patch that does neither is noise at best, contamination at worst.** If you cannot answer "this deepens authority" or "this reduces drift" for your patch, stop and reconsider the approach before submitting.

### Quick self-check before submitting a patch

```
[ ] Q1 answered: real state change named (not just stdout)
[ ] Q2 answered: authoritative stage named (Section 2)
[ ] Q3 answered: sabotage condition described, exit code confirmed
[ ] Q4 answered: invariant referenced (Section 4)
[ ] Q5 answered: legacy path removed or confirmed absent
[ ] Q6 answered: receipt/test/OTEL span cited as proof
[ ] Patch deepens authority OR reduces drift (Section 6)
```

If any box is unchecked, the patch is incomplete.

---

**See also:**
- [Andon Signals](andon/signals.md) — stop-the-line protocol
- [OTEL Validation](otel-validation.md) — proof via spans for LLM/external services
- [Testing](rust/testing.md) — Chicago TDD; real collaborators for sabotage tests
- `crates/ggen-receipt/src/lib.rs` — receipt signing and verification
- `crates/ggen-marketplace/src/rdf/control.rs` — profile enforcement
- `crates/ggen-cli/src/cmds/sync.rs` — sync command authoritative path
