# CCS construct_hash_gate — tip-cargo ALIVE receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CCS-CONSTRUCT-ALIVE-STANDING` |
| Branch | `cursor/v2711-ccs-construct-alive-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.2 CCS #3 (`construct_hash_gate`) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `f1492ed5563b29c7065d188bceac6156c3397ce2` (`678a6c106+`) |
| Repo | `seanchatmangpt/ggen` |
| CCS #3 path (`corroborate_hashes` in-lib courts) | **ALIVE** |
| Clause-2 / full A2A lifecycle crown | **PARTIAL_ALIVE** (`lifecycle.rs` **PRESENT**; lifecycle package courts open) |

**Law:** Tip-cargo **4 passed; exit 0** on the pure CP17 refuse courts in `construct_hash_gate.rs` promotes CCS #3 to **ALIVE** on the admitted tip. BRCE / broker rows remain in [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md).

**Falsifier (wrong filter):** `cargo test -p ggen-lsp --lib construct_hash_gate` (without `--features a2a` or without matching the `a2a_mcp::construct_hash_gate::tests::*` name path) → **0 passed; N filtered** — not evidence of absence; **REFUSED_UNGROUNDED** if promoted to `ALIVE`.

---

## Tip-cargo evidence

| Command | Result | Standing |
|---------|--------|----------|
| `CARGO_BUILD_JOBS=1 cargo test -p ggen-lsp --features a2a --lib a2a_mcp::construct_hash_gate::tests --target-dir /tmp/ggen-ccs-construct-alive-601f-target --config build.jobs=1 -q -- --test-threads=1` | **4 passed; 0 failed; exit 0** (log `/tmp/construct-hash-gate-correct.log` 2026-11-22; loadavg_1m≈6.6 at start) | **ALIVE** |

Tests exercised (all **ok**):

- `empty_expected_hash_is_refused`
- `whitespace_expected_hash_is_refused`
- `fabricated_hash_is_refused`
- `matching_hash_admits_construct_seal`

Source: `crates/ggen-lsp/src/a2a_mcp/construct_hash_gate.rs` (`#[cfg(test)] mod tests`).

---

## Replay (exact)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git checkout origin/cursor/ecosystem-spec-27-1-1-bc54  # @ f1492ed55+
CARGO_BUILD_JOBS=1 cargo test -p ggen-lsp --features a2a --lib a2a_mcp::construct_hash_gate::tests \
  --target-dir /tmp/ggen-ccs-construct-alive-601f-target --config build.jobs=1 -q -- --test-threads=1
# expect: 4 passed; exit 0
```

Shorter equivalent filter (same four tests): `construct_hash_gate::tests` (still requires `--features a2a`).

---

## Compact return line

```text
tip=f1492ed5563b29c7065d188bceac6156c3397ce2
filter=a2a_mcp::construct_hash_gate::tests
result=4_passed_exit_0
landed=cursor/v2711-ccs-construct-alive-bc54
next=CCS#2 write_apply tip-cargo (ggen-mcp courts); lifecycle package courts if not yet green on tip
```