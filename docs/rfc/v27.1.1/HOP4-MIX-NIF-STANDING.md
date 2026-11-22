# Hop-4 mix / NIF residual — standing receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `HOP4-MIX-NIF-STANDING-27.1.1` |
| Branch | `cursor/v2711-hop4-mix-nif2-bc54` |
| Companion | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 4, [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md), [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) G2-10 residual, [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) |
| Ecospec tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `513704d04` |
| igniter mount | `/home/ubuntu/ggen_igniter` @ `b5a15528eb6792dd366fe744ca87f24a80cc46fc` |
| Host | Linux x86_64 cloud VM |
| Status | LEDGER (not path-crown `ALIVE`; hop-4 BrandTheme manufacture remains **ALIVE**; NIF build now **ALIVE**) |
| Evidence | `/tmp/cursor/hop4-mix-nif2-probe.md`; log `/tmp/cursor/hop4-mix-nif2.log`; prior `/tmp/cursor/hop4-mix-nif-probe.md` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

Source presence and off-PATH binaries are **never** promoted to Jobs/C4 `ALIVE`.
NIF `.so` materialization alone is **never** promoted to Jobs/C4 `ALIVE`.

---

## 1. Crown claim (residual only)

```text
ALIVE(mix ggen_igniter.sync --check)
  ⇔  observed: in checkout seanchatmangpt/ggen_igniter,
               exclusive CXX=g++-13 NIF build succeeds,
               `mix ggen_igniter.sync --check` → exit 0
```

Hop-4 BrandTheme manufacture compare is a **separate** crown (already **ALIVE**).
This ledger closes only the NIF / Jobs/C4 residual.

---

## 2. Probe matrix (nif2 land — no tip-lock thrash)

| Probe | Result | Standing contribution |
|-------|--------|------------------------|
| `command -v mix` / `elixir` on default PATH | **ABSENT** | Jobs/C4 default PATH **ABSENT** |
| Export off-PATH `1.19.0-otp-27` + OTP `27.1.2` onto PATH | **PRESENT** (Mix 1.19.0 / OTP 27.1.2) | Toolchain materialization **SUPPORTED** |
| `loadavg_1m` at start | **6.07** (≤8) | Attempt gate **PASS** |
| Tip locks (`ggen-tip-exclusive` / `uilayout-tip-cargo`) | **Intact** (not killed) | No tip thrash |
| Separate dirs | `CARGO_TARGET_DIR=/tmp/igniter-nif-cargo-target-gxx13-nif2`; `MIX_BUILD_PATH=/tmp/igniter-mix-build-nif2` | Isolated from tip targets |
| `CXX=g++-13` + `CARGO_BUILD_JOBS=1` NIF | **ALIVE** — `priv/native/ggen_graph_nif.so` copied from release `libggen_graph_nif.so` | Closes prior oxrocksdb **SIGTERM** BLOCKED |
| First `mix compile` | exit **1** (`Ash.PlugHelpers` race in `ash_json_api`) | Non-crown; sync path recompiled |
| Bare `mix ggen_igniter.sync --check` | exit **2** `REFUSED:INVOCATION --template is required …` | Crown open — **REFUSED_INVOCATION** |
| Pin `.tool-versions` `elixir 1.20.4-otp-29` | **ABSENT** on host | Pin drift vs materialization |
| apt `elixir` 1.14 | Available | **UNSUPPORTED** vs `~> 1.17` |

---

## 3. Is the NIF build path documented?

| Source | What it documents |
|--------|-------------------|
| `ggen_igniter` `CONTRIBUTING.md` / `CLAUDE.md` / `docs/contributing/dev-setup.md` | First `mix compile` builds Rustler NIF `native/ggen_graph_nif`; doctor checks 14–15 |
| `ggen_igniter` `native/CLAUDE.md` + `native/ggen_graph_nif/Cargo.toml` | Rustler + oxigraph `=0.5.11` (pulls `oxrocksdb-sys`) |
| ggen RFC residual (this family) | exclusive `CXX=g++-13` → `mix ggen_igniter.sync --check` → **0** |
| `mix help ggen_igniter.sync` / Chicago `ggen_igniter_sync_check_test.exs` | `--check` still requires `--template` or `--pack`/`--pack-dir` with exactly one template |

NIF path is **documented** and now **executed ALIVE** on this land.
Bare `--check` without template/pack is a typed **REFUSED:INVOCATION** (exit 2) — not a missing NIF.

---

## 4. Can Linux materialize `mix` without founder HITL?

| Path | Standing | Notes |
|------|----------|-------|
| Export off-PATH elixir-install `bin` onto PATH | **SUPPORTED** / **executed** | No founder HITL |
| `curl` elixir-lang `install.sh` → pin 1.20.4-otp-29 | **SUPPORTED** | Egress allow-all; no founder secret |
| `sudo apt-get install elixir` (1.14) | **UNSUPPORTED** for this subject | Below `~> 1.17` |
| Founder HITL for `mix` binary alone | **Not required** | Distinct from foundry model-access / WebAuthn HITL |

Antifalsifier: materializing `mix` ≠ Jobs/C4 `ALIVE`. NIF `.so` ≠ Jobs/C4 `ALIVE`. Crown still needs `--check` exit **0**.

---

## 5. Standing receipt

| Claim | Standing |
|-------|----------|
| Default PATH `mix` / `elixir` | **ABSENT** |
| Off-PATH elixir-install on PATH (1.19.0-otp-27) | **PRESENT** (exported this land) |
| Pin `.tool-versions` 1.20.4-otp-29 | **ABSENT** (installable without HITL) |
| apt elixir 1.14 as subject toolchain | **UNSUPPORTED** |
| NIF build docs | **PRESENT** |
| `CXX=g++-13` exclusive NIF build | **ALIVE** (`ggen_graph_nif.so` produced; tip locks untouched) |
| Bare `mix ggen_igniter.sync --check` | **REFUSED_INVOCATION** (exit **2**; `--template`/`--pack` required) |
| Hop-4 BrandTheme manufacture | **ALIVE** (unchanged; see [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md)) |
| Jobs/C4 / mix sync residual crown | **REFUSED_INVOCATION** (NIF no longer BLOCKED; crown open) |

---

## 6. Exact next verifier

NIF is closed. Crown still needs an admitted sync subject:

```bash
export PATH="$HOME/.elixir-install/installs/elixir/1.19.0-otp-27/bin:$HOME/.elixir-install/installs/otp/27.1.2/bin:$PATH"
cd /home/ubuntu/ggen_igniter   # @ b5a15528+
export CXX=g++-13
export CARGO_TARGET_DIR=/tmp/igniter-nif-cargo-target-gxx13-nif2   # reuse warm NIF
export MIX_BUILD_PATH=/tmp/igniter-mix-build-nif2
# Admit a pack or explicit template (bare --check alone → REFUSED:INVOCATION):
mix ggen_igniter.sync --pack <admitted-pack> --check
# or: --ontology … --query … --template … --out … --check
# expect: exit 0 for residual ALIVE
```

Do **not** invent Jobs/C4 `ALIVE` from NIF `.so` alone or from exit-2 `REFUSED:INVOCATION`.
Do **not** kill tip exclusive locks to force a quieter window.

## Replay (honesty only)

```bash
command -v mix; echo mix_default_exit:$?
test -x "$HOME/.elixir-install/installs/elixir/1.19.0-otp-27/bin/mix"
test -x /usr/bin/g++-13
test -f /home/ubuntu/ggen_igniter/priv/native/ggen_graph_nif.so
test -f docs/rfc/v27.1.1/HOP4-MIX-NIF-STANDING.md
rg -n 'check_exit:2|REFUSED:INVOCATION|libggen_graph_nif.so' /tmp/cursor/hop4-mix-nif2.log
```
