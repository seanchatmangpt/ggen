# CURVE25519-DALEK-PIN-STANDING — Gate-1 dalek lock pin / policy

| Field | Value |
|-------|-------|
| Document ID | `CURVE25519-DALEK-PIN-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` §12; `GATE-1-GAP-MATRIX.md` row `curve25519-dalek` |
| Subject | Workspace `curve25519-dalek` / `ed25519-dalek` lock pin |
| Tip pin (observation) | `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` (`origin/cursor/ecosystem-spec-27-1-1-bc54`; ⊇ `49ebdb27c+`; prior court land `bebea5dd0`) |
| Court land | `cursor/v2711-curve25519-court-bc54` (sidebranch; no crown push) |
| Honesty retip | `cursor/v2711-curve25519-retip-bc54` (sidebranch; no crown absorb) |
| Materialized | 2026-11-22 |
| Repo scope | `seanchatmangpt/ggen` (docs + lock census + policy court) |

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
| Lock resolves documented 4.1.3 + 5.0.0 | **ALIVE** | tip `Cargo.lock` census @ `bebea5dd0` |
| Dedicated Gate-1 pin/policy doc present | **ALIVE** | this document |
| Policy court refuses undocumented major | **ALIVE** | `curve25519_dalek_pin_court` **6 passed; EXIT 0** @ tip `bebea5dd0` |
| Crown crypto / AS-01 / Plane-5 | **UNSUPPORTED** from this pin | not this subject's claim |

```text
PARTIAL_ALIVE(curve25519-dalek Gate-1 pin)
  ⇔  Cargo.lock contains exact versions 4.1.3 and 5.0.0
  ∧  this standing doc cites tip pin + dependents + policy

ALIVE(curve25519-dalek Gate-1 pin court)
  ⇔  PARTIAL_ALIVE above
  ∧  cargo test -p ggen-config --test curve25519_dalek_pin_court
     exits 0 on the subject tip
     (refuse undocumented major; exact set {4.1.3, 5.0.0})
```

---

## 5. Next exact verifiers

Policy court (preferred Gate-1 falsifier):

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER=
CARGO_TARGET_DIR=/tmp/ggen-curve25519-court \
  cargo test -p ggen-config --test curve25519_dalek_pin_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 6 passed; EXIT 0
```

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

**ALIVE pinned:** tip-wins absorb of `cursor/v2711-curve25519-court-bc54` @ `615b8cb7a` onto crown tip `bebea5dd0` via `cursor/v2711-oor15-curve-absorb-ded8`; absorb replay **6 passed; EXIT 0** (`/tmp/ggen-curve-absorb-court`). Silent major drift still fails closed via `REFUSED_UNDOCUMENTED_MAJOR` until this standing + gap-matrix bump in the same change. Crown aggregate Gate-1 remains separate — this subject does not mint crown crypto / AS-01 / Plane-5 `ALIVE`.

---

## 6. Receipt (this land)

| Item | Value |
|------|-------|
| Tip pin SHA | `bebea5dd0d4c9af533457e04e0ed068ae8844f69` |
| Sidebranch | `cursor/v2711-curve25519-court-bc54` |
| Doc path | `docs/rfc/v27.1.1/CURVE25519-DALEK-PIN-STANDING.md` |
| Court | `crates/ggen-config/tests/curve25519_dalek_pin_court.rs` |
| Court result | **6 passed; 0 failed; EXIT 0** (`/tmp/ggen-curve25519-court`; log `/tmp/cursor/curve25519-dalek-pin-court.log`) |
| Lock versions | curve25519-dalek **4.1.3** + **5.0.0**; ed25519-dalek **2.2.0** + **3.0.0** |
| Standing | **ALIVE** (lock + pin doc + policy court EXIT 0 on tip) |
| Crown push / ManagePullRequest | **none** (sidebranch only) |

---

## 7. Honesty retip (2026-11-22) — `cursor/v2711-curve25519-retip-bc54`

| Field | Value |
|-------|-------|
| Tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `167352d0d22a6c90836fdf707a6eb3fbb2b1ee8f` (`167352d0d`; `49ebdb27c+`; tip-wins Truex exact-tip4 absorb over OOR retip17) |
| Sidebranch | `cursor/v2711-curve25519-retip-bc54` |
| Agent | `bc-7a10d89f` (`https://cursor.com/agents/bc-7a10d89f-b1c4-591c-a8b9-701c0f2c5302`) |
| UTC | `2026-11-22T17:36:18Z` (court); honesty land after tip race |
| Crown absorb | **none** (sidebranch only; no ManagePullRequest; no force-push) |
| Foundry | **not claimed** — this retip does **not** mint Foundry `ALIVE` |

### Observed (this retip)

| Probe | Result |
|-------|--------|
| Lock census `curve25519-dalek` | **4.1.3** + **5.0.0** (checksums unchanged vs §1) |
| Lock census `ed25519-dalek` | **2.2.0** + **3.0.0** |
| Policy court | `cargo test -p ggen-config --test curve25519_dalek_pin_court --locked` → **6 passed; 0 failed; EXIT 0** |
| Target dir / log | `/tmp/ggen-curve25519-retip-court`; `/tmp/cursor/curve25519-dalek-pin-court-retip.log` |
| Status file | `/tmp/cursor/curve25519-dalek-pin-court-retip.status` → `EXIT:0` |

### Standing (this retip) — no false Foundry `ALIVE`

| Claim | Standing |
|-------|----------|
| Lock resolves documented 4.1.3 + 5.0.0 @ tip `167352d0d` | **ALIVE** |
| Policy court refuses undocumented major | **ALIVE** — **6 passed; EXIT 0** on tip base |
| Gate-1 `curve25519-dalek` pin (subject) | **ALIVE** (reconfirm; prior land `bebea5dd0`) |
| Crown crypto / AS-01 / Plane-5 | **UNSUPPORTED** from this pin (unchanged) |
| Foundry / zero-HITL / hop-5 | **not this subject** — refuse Foundry `ALIVE` |

```text
ALIVE(curve25519-dalek Gate-1 pin court @ tip 167352d0d)
  ⇔  Cargo.lock exact set {4.1.3, 5.0.0}
  ∧  cargo test -p ggen-config --test curve25519_dalek_pin_court
     → 6 passed; EXIT 0
  ∧  this honesty retip cites tip base + EXIT + sidebranch
```

**Replay:**

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER=
CARGO_TARGET_DIR=/tmp/ggen-curve25519-retip-court \
  cargo test -p ggen-config --test curve25519_dalek_pin_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 6 passed; EXIT 0
```

---

## 8. Honesty retip2 (2026-11-22) — `cursor/v2711-curve25519-retip2-bc54`

| Field | Value |
|-------|-------|
| Tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `c2cd54f562f1fe675d9ea5fabf989b89a9f8a796` (≈ `167352d0d+`; ⊇ tip-wins absorb of retip @ `810a88912`) |
| Sidebranch | `cursor/v2711-curve25519-retip2-bc54` |
| Agent | `bc-cf179f93-ea03-5661-841d-7d083467b897` (`https://cursor.com/agents/bc-cf179f93-ea03-5661-841d-7d083467b897`) |
| UTC | `2026-11-22T17:46:03Z` |
| Crown absorb | **none** (sidebranch only; no ManagePullRequest; no force-push) |
| Foundry | **not claimed** — this retip does **not** mint Foundry `ALIVE` |

### Observed (this retip)

| Probe | Result |
|-------|--------|
| Lock census `curve25519-dalek` @ tip | **4.1.3** + **5.0.0** |
| Lock census `ed25519-dalek` | **2.2.0** + **3.0.0** |
| Court sources vs tip | ≡ (only this standing doc drifted since `167352d0d`) |
| Policy court | `cargo test -p ggen-config --test curve25519_dalek_pin_court --locked` → **6 passed; 0 failed; EXIT 0** in **141s** (&lt;3min) |
| Target dir / log | `/tmp/ggen-curve25519-retip2-court`; `/tmp/cursor/curve25519-retip2-court.log` |
| Status file | `/tmp/cursor/curve25519-retip2-court.status` → `EXIT:0 elapsed=141s` |

### Standing (this retip) — no false Foundry `ALIVE`

| Claim | Standing |
|-------|----------|
| Lock resolves documented 4.1.3 + 5.0.0 @ tip `c2cd54f562f1fe675d9ea5fabf989b89a9f8a796` | **ALIVE** |
| Policy court refuses undocumented major | **ALIVE** — **6 passed; EXIT 0** (&lt;3min) |
| Gate-1 `curve25519-dalek` pin (subject) | **ALIVE** (retip2 reconfirm) |
| Crown crypto / AS-01 / Plane-5 | **UNSUPPORTED** from this pin |
| Foundry / zero-HITL / hop-5 | **not this subject** — refuse Foundry `ALIVE` |

```text
ALIVE(curve25519-dalek Gate-1 pin court @ tip c2cd54f562f1fe675d9ea5fabf989b89a9f8a796)
  ⇔  Cargo.lock exact set {4.1.3, 5.0.0}
  ∧  cargo test -p ggen-config --test curve25519_dalek_pin_court
     → 6 passed; EXIT 0 (&lt;3min)
  ∧  this honesty retip2 cites tip base + EXIT + sidebranch
```

**Replay:**

```bash
export CARGO_BUILD_JOBS=1 CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER=
CARGO_TARGET_DIR=/tmp/ggen-curve25519-retip2-court \
  cargo test -p ggen-config --test curve25519_dalek_pin_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 6 passed; EXIT 0
```
