# Hop-4 mix / NIF residual — standing receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `HOP4-MIX-NIF-STANDING-27.1.1` |
| Branch | `cursor/v2711-hop4-mix-check-alive-bc54` (prior nif2 `cursor/v2711-hop4-mix-nif2-bc54` @ `77095a8d8`) |
| Companion | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 4, [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md), [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) G2-10 residual, [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) |
| Ecospec tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `f1492ed55` (past nif2 absorb `ebf4e8d08` / `513704d04`) |
| igniter mount | `/home/ubuntu/ggen_igniter` @ `b5a15528eb6792dd366fe744ca87f24a80cc46fc` |
| Host | Linux x86_64 cloud VM |
| Status | LEDGER — Jobs/C4 residual crown **ALIVE** (admitted `--pack-dir`/`--template` + `--check` exit **0**); hop-4 BrandTheme manufacture remains **ALIVE**; path crown still **PARTIAL** |
| Evidence | `/tmp/cursor/hop4-mix-check-alive-probe.md`; log `/tmp/cursor/hop4-mix-check-alive.log`; prior nif2 `/tmp/cursor/hop4-mix-nif2-probe.md` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

Source presence and off-PATH binaries are **never** promoted to Jobs/C4 `ALIVE`.
NIF `.so` materialization alone is **never** promoted to Jobs/C4 `ALIVE`.
Bare `--check` without `--pack`/`--template` remains a typed **REFUSED:INVOCATION** (exit 2) — not Jobs/C4 `ALIVE`.

---

## 1. Crown claim (residual only)

```text
ALIVE(mix ggen_igniter.sync --check)
  ⇔  observed: in checkout seanchatmangpt/ggen_igniter,
               exclusive CXX=g++-13 NIF build succeeds,
               admitted --pack/--pack-dir/--template subject,
               `mix ggen_igniter.sync … --check` → exit 0
```

Hop-4 BrandTheme manufacture compare is a **separate** crown (already **ALIVE**).
This ledger closes the NIF / Jobs/C4 residual.

---

## 2. Probe matrix (check-alive land — reuse nif2 NIF)

| Probe | Result | Standing contribution |
|-------|--------|------------------------|
| `command -v mix` / `elixir` on default PATH | **ABSENT** | Jobs/C4 default PATH **ABSENT** |
| Export off-PATH `1.19.0-otp-27` + OTP `27.1.2` onto PATH | **PRESENT** (Mix 1.19.0 / OTP 27.1.2) | Toolchain materialization **SUPPORTED** |
| Tip locks (`ggen-tip-exclusive` / `uilayout-tip-cargo`) | **Intact** (not killed) | No tip thrash |
| Reuse dirs | `CARGO_TARGET_DIR=/tmp/igniter-nif-cargo-target-gxx13-nif2`; `MIX_BUILD_PATH=/tmp/igniter-mix-build-nif2`; `LD_LIBRARY_PATH=…/priv/native` | Warm NIF; isolated from tip cargo |
| `CXX=g++-13` NIF `.so` | **ALIVE** / **PRESENT** (`priv/native/ggen_graph_nif.so`) | Prior nif2 close retained |
| Bare `mix ggen_igniter.sync --check` | exit **2** `REFUSED:INVOCATION` (prior nif2) | Correct refusal — not crown |
| Admitted `--pack-dir test/fixtures/sample-pack` sync write | exit **0** | Setup for clean check |
| Admitted `--pack-dir test/fixtures/sample-pack … --check` | exit **0** `clean (0 drifted files)` | **Jobs/C4 ALIVE** |
| Chicago explicit `--ontology/…/--template … --check` | exit **0** `clean (0 drifted files)` | Replay alt **ALIVE** |

---

## 3. Admitted invocation (Chicago)

Bare `--check` alone is **REFUSED:INVOCATION**. Crown requires an admitted pack or template subject (exactly one template via `--pack`/`--pack-dir`, or explicit `--template`):

```bash
export PATH="$HOME/.elixir-install/installs/elixir/1.19.0-otp-27/bin:$HOME/.elixir-install/installs/otp/27.1.2/bin:$PATH"
cd /home/ubuntu/ggen_igniter   # @ b5a15528+
export CXX=g++-13
export CARGO_TARGET_DIR=/tmp/igniter-nif-cargo-target-gxx13-nif2
export MIX_BUILD_PATH=/tmp/igniter-mix-build-nif2
export LD_LIBRARY_PATH="/home/ubuntu/ggen_igniter/priv/native:${LD_LIBRARY_PATH:-}"
ROOT=/tmp/hop4-mix-check-alive-555967

# Setup write (clean subject for --check):
mix ggen_igniter.sync \
  --pack-dir test/fixtures/sample-pack \
  --out "$ROOT/out/resource.ex" \
  --manifest-dir "$ROOT" \
  --verify-cwd "$(pwd)"
# sync_exit:0

# Crown:
set -o pipefail
mix ggen_igniter.sync \
  --pack-dir test/fixtures/sample-pack \
  --out "$ROOT/out/resource.ex" \
  --manifest-dir "$ROOT" \
  --verify-cwd "$(pwd)" \
  --check
# check_exit:0 → "ggen_igniter.sync --check: clean (0 drifted files)"
```

Alt (Chicago `ggen_igniter_sync_check_test.exs` args): `--ontology` + four `--query` + `--template test/fixtures/extension.ex.eex` + `--out` + `--check` → **check2_exit:0**.

---

## 4. Standing receipt

| Claim | Standing |
|-------|----------|
| Default PATH `mix` / `elixir` | **ABSENT** |
| Off-PATH elixir-install on PATH (1.19.0-otp-27) | **PRESENT** (exported) |
| `CXX=g++-13` exclusive NIF build | **ALIVE** (`ggen_graph_nif.so`) |
| Bare `mix ggen_igniter.sync --check` | **REFUSED_INVOCATION** (exit **2**; antifalsifier) |
| Admitted `--pack-dir sample-pack … --check` | **ALIVE** (exit **0**) |
| Admitted `--template` Chicago `--check` | **ALIVE** (exit **0**) |
| Hop-4 BrandTheme manufacture | **ALIVE** (unchanged) |
| Jobs/C4 / mix sync residual crown | **ALIVE** |
| Photolithography path crown | **PARTIAL** (hop-5 foundry `--live` still **REFUSED**) |
| OOR (cos/billing/WebAuthn/Plane-5) | **BLOCKED** / **UNSUPPORTED** (unchanged; not this land) |

---

## 5. Exact next verifier

This residual is **closed**. Replay only:

```bash
# same admitted pack-dir --check block in §3 → expect exit 0
rg -n 'check_exit:0|check2_exit:0|clean \(0 drifted' /tmp/cursor/hop4-mix-check-alive.log
```

Path-crown / hop-5 next remains foundry `--live` → **0** after provider creds (not this residual).
Do **not** invent path-crown or OOR `ALIVE` from Jobs/C4 close alone.
Do **not** kill tip exclusive locks.

## Replay (honesty only)

```bash
command -v mix; echo mix_default_exit:$?
test -x "$HOME/.elixir-install/installs/elixir/1.19.0-otp-27/bin/mix"
test -f /home/ubuntu/ggen_igniter/priv/native/ggen_graph_nif.so
test -f docs/rfc/v27.1.1/HOP4-MIX-NIF-STANDING.md
rg -n 'check_exit:0|ALIVE' docs/rfc/v27.1.1/HOP4-MIX-NIF-STANDING.md
```
