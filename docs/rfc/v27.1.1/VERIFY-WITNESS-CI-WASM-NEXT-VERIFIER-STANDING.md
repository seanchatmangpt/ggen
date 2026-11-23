# `verify_witness` production CI-wasm — exact next verifier pin (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING-27.1.1` |
| Branch | side `cursor/v2711-vw-ci-wasm-retip-bc54` from `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `f12972590` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §10 / Gate-1 gap row `verify_witness WASM < 250KB`; OOR **G2-11** |
| Companions | [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md); [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md); [`VERIFY-WITNESS-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-SIZE-COURT-STANDING.md); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) **G2-11**; [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md); [`CROWN-PROGRESS-RECEIPT.md`](./CROWN-PROGRESS-RECEIPT.md) |
| Observed ggen tip (side base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `f12972590e280609ddaedeafad84cda63372ea4b` (exact-tip retip; ⊇ retip2 `893070836` / env-wire / affidavit) |
| Env-wire evidence | tip-wins `b2f6e12d2` / crown code `f65434315` — Chicago **8/8** EXIT **0** held; `AFFIDAVIT_VERIFY_WITNESS_SIZE_BUDGET_BYTES=256_000` |
| Affidavit pin evidence | `563eb322a16d805ebc99b2c16f1165462244d7e2` — `wc -c` **28673**; SHA-256 `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` (held; pin court not re-run this land) |
| Cargo / tip-cargo | size-court cargo **BUILD_BROKEN** this retip — cgroup OOM / tip-exclusive thrash (**EXIT 137**); prior lineage **8/8** held; **not** a production substitute |
| Status | LEDGER — env wire **PARTIAL**; affidavit pin size court **ALIVE** (bytes held); CI mount fixture **PRESENT** on `cursor/v2711-vw-ci-consume-bc54` (`affidavit_gatekeeper_verify_witness.pin.wasm` + workflow); production runtime **PENDING_CI** until workflow EXIT **0** (was **UNSUPPORTED**); **not** production `ALIVE` until CI green; Foundry **not** promoted. Companion: [`VERIFY-WITNESS-CI-WASM-CONSUME-STANDING.md`](./VERIFY-WITNESS-CI-WASM-CONSUME-STANDING.md) |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document pins the **exact next verifier** for ggen production
`verify_witness` runtime (OOR **G2-11**) so a later crown absorb can cite one closed
CI-mount + consume court. It does **not** rewrite Gate-1 / OOR tip-pin headers. It
does **not** promote env-wire **PARTIAL**, placeholder size-court **ALIVE**, or
affidavit pin size-court **ALIVE** to ggen production runtime `ALIVE`.

**Honesty law (held):**
`size_court_ALIVE` ≠ `env_wire_PARTIAL` ≠ `affidavit_pin_ALIVE` ≠ `production_runtime_ALIVE`.
**Do not invent production `ALIVE`.**

---

## 1. Standing pin (honesty)

| Claim | Standing | Evidence (held; not re-run this land) |
|-------|----------|---------------------------------------|
| ggen placeholder / size+env Chicago court | tip-cargo **ALIVE** (**8** @ `b2f6e12d2` → crown `f65434315`; prior size-only **6** @ `f5f44571f`) | `cargo test -p ggen-graph --test verify_witness_size_court` — size fence + env courts only |
| ggen `GGEN_VERIFY_WITNESS_WASM` env wire + `< 256_000` budget | **PARTIAL** | Code PRESENT (`load_production_verify_witness` / `verify_production_witness`); Chicago **8/8** EXIT **0**; CI env **unset** → typed `WitnessRefusal::Unsupported` |
| Affidavit gatekeeper production pin size court | **ALIVE** | EXIT **0**; `wc -c` **28673** @ `563eb322a`; bound ≤ **256000**; SHA `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` |
| Affidavit checkout in this worktree | **ABSENT** | production path out of ggen |
| `wasm32-unknown-unknown` rustup target in ggen CI image | **ABSENT** (x86_64 host only observed) | rebuild path not required when pin artifact is mounted |
| `GGEN_VERIFY_WITNESS_WASM` in production / CI default | **unset** | env wire PARTIAL ≠ CI consume |
| CI-mounted real `affidavit_gatekeeper_verify_witness.wasm` | **ABSENT** | no CI artifact mount observed |
| **ggen production `verify_witness` runtime** | **UNSUPPORTED** | this pin — §3; no CI-mounted real wasm + consume EXIT **0** |

```text
base=origin/cursor/ecosystem-spec-27-1-1-bc54@f12972590
side=cursor/v2711-vw-ci-wasm-retip-bc54
env_wire=PARTIAL (GGEN_VERIFY_WITNESS_WASM + 256KB budget; lineage court 8/8 held)
affidavit_pin=ALIVE (28673B SHA 5faae464… held; pin 563eb322a)
production_runtime=UNSUPPORTED (GGEN_VERIFY_WITNESS_WASM unset; CI mount ABSENT)
size_court_cargo=BUILD_BROKEN (EXIT 137 cgroup OOM / tip thrash)
honesty=env_wire_PARTIAL_neq_affidavit_ALIVE_neq_production_ALIVE
ManagePullRequest=not_used
crown_push=REFUSED (sidebranch only)
Foundry=not_ALIVE
```

---

## 2. Owner / surface

| Role | Owner |
|------|-------|
| Exact next production-runtime converge | **CI + Founder HITL** — mount real gatekeeper wasm into ggen CI and set `GGEN_VERIFY_WITNESS_WASM` |
| Production WASM bytes / size court | **affidavit** (`affidavit-gatekeeper-wasm` @ pin `563eb322a`) — out of ggen |
| Env path override / consume API | **ggen** (`ggen_graph::witness::{load_production_verify_witness,verify_production_witness}`) |
| Placeholder size fence (non-substitute) | **ggen** — Chicago `verify_witness_size_court` (**8**) |
| OOR row | **G2-11** ([`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md)) |
| Path ledger | [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) |

**Sequencing law (held):** affidavit pin size court **ALIVE** → ggen env wire **PARTIAL** → (§3 CI mount + consume EXIT **0**) → production runtime may promote. Until §3 exits **0**, production remains **UNSUPPORTED**. Do **not** skip to production `ALIVE` from placeholder courts, unset-env typed refuse, or affidavit pin alone.

---

## 3. Exact next verifier (CI mount + consume)

**Falsifier for ggen production `verify_witness` runtime (G2-11):** the CI court
below must exit **0** with a **real** mounted gatekeeper wasm (pin **28673** B @
`563eb322a` or later tip retaining that pin) and `GGEN_VERIFY_WITNESS_WASM` set.
Until then standing stays **UNSUPPORTED** — **not** `ALIVE`.

Success on this path still yields API standing `WitnessStanding::PartialAlive` /
`ArtifactProvenance::ExternalPath` (CONSTRUCT court). That CONSTRUCT receipt is
the required consume EXIT **0** for production-runtime promotion on the ledger —
it is **not** Plane-4 DO and must not be narrated as DO `ALIVE`.

```bash
# CI / Founder HITL — mount real affidavit gatekeeper wasm into ggen CI
# Owner: affidavit bytes + ggen CI consume. Agent must not invent wasm bytes.
#
# Preconditions (admit — do not invent):
#   1) Affidavit pin size court ALIVE @ 563eb322a (held):
#        just gatekeeper-wasm-size-court  → EXIT 0
#        wc -c …/affidavit_gatekeeper_verify_witness.wasm → 28673
#   2) Mount the REAL artifact into the ggen CI job filesystem
#      (checkout, cache, or artifact download — NOT the 35 B placeholder).
#   3) Export absolute path:
#        export GGEN_VERIFY_WITNESS_WASM=/abs/path/to/affidavit_gatekeeper_verify_witness.wasm
#
# Step A — prove mounted bytes match the production pin (fail-closed):
set -o pipefail
test -f "$GGEN_VERIFY_WITNESS_WASM"
wc -c "$GGEN_VERIFY_WITNESS_WASM"          # expect 28673 (pin) or later pin retaining ≤256000
sha256sum "$GGEN_VERIFY_WITNESS_WASM"      # pin: 5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97
# refuse fat affidavit-wasm (718736 B) — that is NOT the gatekeeper product

# Step B — affidavit pin replay (when affidavit checkout is mounted alongside CI):
#   cd /path/to/affidavit && git checkout 563eb322a16d805ebc99b2c16f1165462244d7e2
#   just gatekeeper-wasm-size-court
#   echo EXIT=$?   # expect 0

# Step C — ggen consume court with env set (CI job; EXIT 0 required):
cd "$(git rev-parse --show-toplevel)"   # ggen @ crown tip under test
CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --target-dir /tmp/ggen-verify-witness-ci-wasm \
  --config build.jobs=1 -- --test-threads=1
echo EXIT=$?
# Additionally: call / assert verify_production_witness(None) against the mounted
# path → Ok(PartialAlive) + ExternalPath; OverBudget (≥256000) → WitnessRefusal::OverBudget;
# unset/missing → WitnessRefusal::Unsupported.

# Pass: Steps A–C typed; process exit 0 on the known-good mounted path with
#       GGEN_VERIFY_WITNESS_WASM set to the real ≤256000 gatekeeper wasm.
# Until then: UNSUPPORTED — no CI-mounted real wasm + consume EXIT 0.
#
# Antifalsifiers (do NOT promote from these):
#   - placeholder 35 B fixture / size court alone          # ≠ production ALIVE
#   - env wire Chicago 8/8 with GGEN_VERIFY_WITNESS_WASM unset
#   - affidavit pin ALIVE @ 563eb322a alone (out-of-ggen)  # ≠ ggen CI consume
#   - remote byte download observation without CI mount
#   - fat affidavit-wasm 718736 B cited as <250KB product  # REFUSED_UNGROUNDED
#   - docs absorb / this markdown file                     # ≠ ALIVE
```

Affidavit pin replay (already EXIT **0** @ `563eb322a` — held; not a ggen CI substitute):

```bash
git clone https://github.com/seanchatmangpt/affidavit.git /tmp/affidavit
cd /tmp/affidavit
git checkout 563eb322a16d805ebc99b2c16f1165462244d7e2
just gatekeeper-wasm-size-court
echo EXIT=$?
wc -c affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
sha256sum affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
```

Expect: EXIT **0**, bytes **28673**, SHA `5faae464…`.

Env-wire / size court replay (PARTIAL fence only — antifalsifier for production):

```bash
set -o pipefail
CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --target-dir /tmp/ggen-verify-witness-env-wire \
  --config build.jobs=1 -- --test-threads=1
echo EXIT=$?
# expect 8 passed; EXIT 0 — still PARTIAL env wire; production remains UNSUPPORTED
```

---

## 4. Explicit non-claims

- This side branch does **not** push `cursor/ecosystem-spec-27-1-1-bc54` (crown).
- This land does **not** claim ggen production `verify_witness` runtime `ALIVE`.
- Env-wire **PARTIAL** (court **8/8**, 256KB budget) does **not** close **G2-11**.
- Affidavit pin size court **ALIVE** (**28673** B @ `563eb322a`) does **not** close ggen CI consume.
- Placeholder size-court **ALIVE** does **not** equal production WASM `ALIVE`.
- Do not absorb this ledger as crown production `ALIVE` — absorb only after the §3
  CI mount + consume court exits **0**, or retain **UNSUPPORTED** with this pin.
- ManagePullRequest is **not** used.

---

## 5. Provenance

| Event | Note |
|-------|------|
| Side-branch pin | 2026-11-22 — `cursor/v2711-vw-ci-wasm-next-bc54` from crown tip `cc2b344bc`; docs-only; env wire **PARTIAL**; affidavit pin **ALIVE** (**28673** B @ `563eb322a`); production runtime **UNSUPPORTED**; exact next = CI mount real wasm + `GGEN_VERIFY_WITNESS_WASM` + consume EXIT **0**; **no** invented production `ALIVE`; no cargo; no ManagePullRequest; **no crown push** |
| Prior evidence chain | placeholder size court tip-cargo → env-wire PARTIAL (`b2f6e12d2` / `f65434315`, **8/8**, 256KB) → affidavit pin size court ALIVE (`563eb322a`, probe `bc-5f5786ff`) → CI-wasm next verifier (this pin; production still **UNSUPPORTED**) |
| Absorb guidance | Tip-wins later onto crown when safe; cite §3 verbatim; standing **UNSUPPORTED** until CI mount + consume EXIT **0** — do **not** invent production `ALIVE` from PARTIAL / pin / docs alone |

---

## 6. Exact-tip retip2 vs tip `893070836` (2026-11-22)

| Field | Value |
|-------|-------|
| Agent | `bc-10c8f83c-8d4c-5129-afad-fd7222733773` |
| Branch | tip-wins absorb `cursor/v2711-vw-ci-wasm-retip2-bc54` @ `4bd09059b` (probe `893070836`; absorb onto live crown; no ManagePullRequest) |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `893070836ef83ab711e657cfacc98bb4dc7ea369` |
| Narrowest court (CI-wasm production falsifier) | CI-mounted real wasm + `export GGEN_VERIFY_WITNESS_WASM=…` + consume EXIT **0** |
| CI mount probe | `GGEN_VERIFY_WITNESS_WASM` **UNSET**; CI-mounted gatekeeper wasm **ABSENT** this env |
| Affidavit bytes (held, not CI-mounted) | **28673**; SHA `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` @ `/tmp/affidavit/affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm` |
| Size-court cargo attempt | `cargo test -p ggen-graph --test verify_witness_size_court` → **EXIT 137** (SIGKILL) under tip-cargo exclusive contention (`/tmp/gate1-exact-tip3/guardian.py`); log `/tmp/cursor/vw-ci-wasm-retip2-court.log` |
| EXIT (production CI-wasm court) | **not runnable** (fail-closed) — mount ABSENT / env UNSET |
| EXIT (size-court cargo) | **137** → **BUILD_BROKEN** this retip (lineage **8/8** PARTIAL held; antifalsifier only) |
| Standing | production runtime **UNSUPPORTED**; env wire **PARTIAL**; affidavit pin **ALIVE** (bytes); size-court cargo **BUILD_BROKEN** this pass |
| Honesty law (held) | `size_court ≠ env_wire_PARTIAL ≠ affidavit_ALIVE ≠ production_ALIVE`; do **not** invent production / Foundry `ALIVE` |
| Exact next verifier | §3 verbatim: CI mount real gatekeeper wasm + `GGEN_VERIFY_WITNESS_WASM` + `cargo test -p ggen-graph --test verify_witness_size_court` consume EXIT **0** |

---

## 7. Exact-tip retip vs tip `f12972590` (2026-11-22) — `cursor/v2711-vw-ci-wasm-retip-bc54`

| Field | Value |
|-------|-------|
| Agent | `bc-83451b55-a213-5c21-860d-1b7239b27391` |
| Branch | `cursor/v2711-vw-ci-wasm-retip-bc54` from exact tip (no crown absorb; no ManagePullRequest) |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `f12972590e280609ddaedeafad84cda63372ea4b` |
| UTC | `2026-11-22T19:01:04Z` |

### Observed probes (this retip)

| Probe | Result |
|-------|--------|
| `GGEN_VERIFY_WITNESS_WASM` | **unset** |
| `.github/workflows` CI mount (`rg` `GGEN_VERIFY_WITNESS_WASM` / `verify_witness.wasm` / `gatekeeper.*wasm`) | **NO_CI_MOUNT_MATCH** |
| `wasm32-unknown-unknown` rustup target | **ABSENT** |
| In-repo wasm | placeholder only — **35** B; SHA `720b97972e97693b197444d46578b75546cd0b0487e7061fc7e1ea45f2953034` |
| Witness source delta vs env-wire ALIVE pin `f65434315` | **empty** (`src/witness` + `verify_witness_size_court.rs` + fixtures) |

### Narrowest real verifier attempt (size/env antifalsifier — not production)

```bash
CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test verify_witness_size_court --locked \
  --target-dir /tmp/ggen-vw-ci-wasm-retip-postguard \
  --config build.jobs=1 -- --test-threads=1
# EXIT=137  (cgroup OOM mid oxrocksdb; log /tmp/cursor/vw-ci-wasm-retip-postguard.log)
```

Prior attempts this agent (same standing; not ALIVE): EXIT **143** (SIGTERM), EXIT **101** (oxrocksdb SIGTERM / shared-target corruption), EXIT **137** (tip-exclusive guardian SIGKILL + cgroup OOM). Production CI-wasm court remain **not runnable** (env UNSET / mount ABSENT).

### Standing (this retip)

| Claim | Standing |
|-------|----------|
| ggen production `verify_witness` runtime (G2-11 / CI-wasm) | **UNSUPPORTED** — **not** promoted; do **not** invent `ALIVE` |
| Env wire (`GGEN_VERIFY_WITNESS_WASM` + `<256000`) | **PARTIAL** (code + lineage **8/8**; CI unset) |
| Affidavit pin size court | **ALIVE** held (**28673** B @ `563eb322a`) |
| Exact-tip size-court cargo | **BUILD_BROKEN** (**EXIT 137**) — lineage **8/8** antifalsifier only |
| Exact next verifier | §3 verbatim: CI mount real gatekeeper wasm + `export GGEN_VERIFY_WITNESS_WASM=…` + consume EXIT **0** |

**Honesty law (held):** `size_court ≠ env_wire_PARTIAL ≠ affidavit_ALIVE ≠ production_ALIVE`. Sidebranch push only; **no** crown absorb; **no** ManagePullRequest on #849; **no** force-push; **no** false `ALIVE`.
