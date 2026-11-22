# CURVE25519-DALEK-PIN-STANDING — Gate-1 dalek lock pin / policy

| Field | Value |
|-------|-------|
| Document ID | `CURVE25519-DALEK-PIN-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` §12; `GATE-1-GAP-MATRIX.md` row `curve25519-dalek` |
| Subject | Workspace `curve25519-dalek` / `ed25519-dalek` lock pin |
| Tip pin (observation) | `9f6fecf83e6a554f946e82b50e8a29275d3dc3e2` (`origin/cursor/ecosystem-spec-27-1-1-bc54`) |
| Materialized | 2026-11-22 |
| Repo scope | `seanchatmangpt/ggen` (docs + lock census only) |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

Never promote inspection of this markdown file alone to `ALIVE`.

---

## 1. Locked versions (Cargo.lock @ tip pin)

| Crate | Exact version | Checksum (crates.io) | Role |
|-------|---------------|----------------------|------|
| `curve25519-dalek` | **4.1.3** | `97fb8b7c4503de7d6ae7b42ab72a5a59857b4c937ec27a3d4539dba95b5ab2be` | Transitive via `ed25519-dalek` **2.2.0** |
| `curve25519-dalek` | **5.0.0** | `b5eed333089e2e1c1ac8c6c0398e5e2497b4c9926ca6d0365ed1e099afa5bc23` | Transitive via `ed25519-dalek` **3.0.0** (workspace admitted path) |
| `ed25519-dalek` | **3.0.0** | `6ebaa1a2bf1290ab3bfe5a7b771d050ebffab2711c19a81691c683a5144a25de` | Workspace `[workspace.dependencies]` pin `version = "3.0"` |
| `ed25519-dalek` | **2.2.0** | `70e796c081cee67dc755e1a36a0a172b897fab85fc3f6bc48307991f64e4eca9` | Transitive (third-party); not the workspace direct pin |

**Admitted direct pin:** root `Cargo.toml` workspace dep

```toml
ed25519-dalek = { version = "3.0", features = ["rand_core"] }
```

`curve25519-dalek` is **not** a direct workspace dependency; majors **4.1.3** and **5.0.0** coexist because `ed25519-dalek` 2.x and 3.x both resolve in the lockfile.

---

## 2. Dependents

### Workspace members → `ed25519-dalek` **3.0.0** → `curve25519-dalek` **5.0.0**

| Crate | Declared in |
|-------|-------------|
| `ggen-sjira` | `crates/ggen-sjira/Cargo.toml` (`workspace = true`) — SovereignLease / epoch |
| `ggen-engine` | `crates/ggen-engine/Cargo.toml` — receipt signing/verification |
| `ggen-graph` | `crates/ggen-graph/Cargo.toml` |
| `ggen-marketplace` | `crates/ggen-marketplace/Cargo.toml` |
| `ggen-config` | `crates/ggen-config/Cargo.toml` |
| `ggen-cli` / `ggen-cli-lib` | `crates/ggen-cli/Cargo.toml` (`"3.0"`) |
| root `ggen` | workspace package |

### Transitive → `ed25519-dalek` **2.2.0** → `curve25519-dalek` **4.1.3**

| Package (lock) | Note |
|----------------|------|
| `graphlaw` | published sibling graph backend |
| `lsp-max` | transitive |
| `purrdf-gts` / `purrdf-rdf` | transitive |
| `wasm4pm-cognition` | transitive |

Archive / example crates under `examples/archive/**` that pin `ed25519-dalek` 2.x are **out of the live workspace members** and do not redefine the Gate-1 admitted path.

---

## 3. Policy (Gate-1)

1. **Documented dual majors are admitted** on tip pin above: 4.1.3 (ed25519-dalek 2.x transitive) + 5.0.0 (ed25519-dalek 3.x workspace path).
2. **Silent major drift is refused:** introducing `curve25519-dalek` **6.x** (or dropping **5.0.0** from the workspace `ed25519-dalek` 3.x path) without updating this standing + gap-matrix row → **REFUSED_UNDOCUMENTED_MAJOR**.
3. **Direct workspace pin stays `ed25519-dalek = "3.0"`** unless a separate Gate-1 admit lands.
4. cgap-aoq / external consumers must cite this doc (or an equivalent cgap-aoq pin) before claiming dalek v4-only or v5-only as the sole admitted surface.
5. SovereignLease Ed25519/`EdDSA` ≠ WebAuthn AS-01/`prime256v1` — see `BRCE-WEBAUTHN-STANDING.md`.

---

## 4. Standing

| Claim | Standing | What would promote |
|-------|----------|--------------------|
| Lock resolves documented 4.1.3 + 5.0.0 | **PARTIAL_ALIVE** | — (already observed) |
| Dedicated Gate-1 pin/policy doc present | **PARTIAL_ALIVE** | this document |
| Policy court refuses undocumented major | **UNKNOWN** / absent | Chicago court (or equivalent CI grep court) that fails closed on lock majors outside `{4.1.3, 5.0.0}` without a doc bump |
| Crown crypto / AS-01 / Plane-5 | **UNSUPPORTED** from this pin | not this subject's claim |

```text
PARTIAL_ALIVE(curve25519-dalek Gate-1 pin)
  ⇔  Cargo.lock contains exact versions 4.1.3 and 5.0.0
  ∧  this standing doc cites tip pin + dependents + policy

ALIVE(curve25519-dalek Gate-1 pin court)
  ⇔  PARTIAL_ALIVE above
  ∧  a named policy court exits 0 on the subject tip
     (refuse undocumented major; see next verifiers)
```

---

## 5. Next exact verifiers

Lock census (no cargo build required):

```bash
rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock
rg -n 'name = "ed25519-dalek"' -A5 Cargo.lock
# expect curve25519-dalek 4.1.3 + 5.0.0; ed25519-dalek 2.2.0 + 3.0.0
```

Workspace direct pin:

```bash
rg -n 'ed25519-dalek' Cargo.toml crates/*/Cargo.toml
# workspace members → 3.0 / workspace = true
```

Lease path still Ed25519 (non-AS-01):

```bash
cargo test -p ggen-sjira --test lease_epoch_test
```

**ALIVE falsifier still missing:** author a Gate-1 policy court (e.g. under `ggen-sjira` or `ggen-config`) that parses `Cargo.lock` for `curve25519-dalek` majors and refuses any set other than the admitted `{4.1.3, 5.0.0}` without a standing-doc bump. Until that court exits 0 on tip, keep standing at **PARTIAL_ALIVE**.

---

## 6. Receipt (this land)

| Item | Value |
|------|-------|
| Tip pin SHA | `9f6fecf83e6a554f946e82b50e8a29275d3dc3e2` |
| Doc path | `docs/rfc/v27.1.1/CURVE25519-DALEK-PIN-STANDING.md` |
| Lock versions | curve25519-dalek **4.1.3** + **5.0.0**; ed25519-dalek **2.2.0** + **3.0.0** |
| Standing | **PARTIAL_ALIVE** (lock + pin doc); policy court → future **ALIVE** |
| Cargo builds this land | **none** (docs-only) |
