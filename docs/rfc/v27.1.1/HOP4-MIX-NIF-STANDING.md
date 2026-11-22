# Hop-4 mix / NIF residual — standing receipt (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `HOP4-MIX-NIF-STANDING-27.1.1` |
| Branch | `cursor/v2711-hop4-mix-nif-bc54` |
| Companion | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) hop 4, [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md), [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) G2-10 residual, [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md) |
| Ecospec tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `2fd897141` (past `46a026ae9`) |
| igniter mount | `/home/ubuntu/ggen_igniter` @ `b5a15528eb6792dd366fe744ca87f24a80cc46fc` |
| Host | Linux x86_64 cloud VM |
| Status | LEDGER (not path-crown `ALIVE`; hop-4 BrandTheme manufacture remains **ALIVE**) |
| Evidence | `/tmp/cursor/hop4-mix-nif-probe.md`; prior NIF logs `/tmp/cursor/hop4-nif-build.log`, `/tmp/cursor/hop4-nif-exclusive.log` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

Source presence and off-PATH binaries are **never** promoted to Jobs/C4 `ALIVE`.

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

## 2. Probe matrix (this land — no cargo thrash)

| Probe | Result | Standing contribution |
|-------|--------|------------------------|
| `command -v mix` / `elixir` on default PATH | **ABSENT** | Jobs/C4 default PATH **ABSENT** |
| `~/.elixir-install/installs/elixir/1.19.0-otp-27/bin/mix` | **PRESENT** (Mix 1.19.0 / OTP 27.1.2) | Off-PATH materialization **PRESENT** |
| `~/.elixir-install/.../1.17.3-otp-27/bin/mix` | **PRESENT** | Same |
| Pin `.tool-versions` `elixir 1.20.4-otp-29` / `erlang 29.1.2` | **ABSENT** on host | Pin drift vs materialization |
| apt candidate `elixir` 1.14.0 | Available | **UNSUPPORTED** vs `mix.exs` `elixir: "~> 1.17"` |
| `g++-13` | **PRESENT** (`/usr/bin/g++-13`) | Residual CXX tool **PRESENT** |
| passwordless `sudo` + egress allow-all | **PRESENT** | Install paths without founder secret |
| Concurrent shared-pod cargo (`ui_layout_court` / `shui`) | Observed | Explains prior `oxrocksdb-sys` **SIGTERM** |
| Prior exclusive NIF (`CXX=g++-13`, `/tmp/igniter-nif-cargo-target-gxx13`) | `oxrocksdb-sys` build-script **SIGTERM** | NIF **BLOCKED** (not missing docs) |

---

## 3. Is the NIF build path documented?

| Source | What it documents |
|--------|-------------------|
| `ggen_igniter` `CONTRIBUTING.md` / `CLAUDE.md` / `docs/contributing/dev-setup.md` | First `mix compile` builds Rustler NIF `native/ggen_graph_nif`; doctor checks 14–15 |
| `ggen_igniter` `native/CLAUDE.md` + `native/ggen_graph_nif/Cargo.toml` | Rustler + oxigraph `=0.5.11` (pulls `oxrocksdb-sys`) |
| ggen RFC residual (this family) | exclusive `CXX=g++-13` → `mix ggen_igniter.sync --check` → **0** |

NIF path is **documented**. Failure mode here is resource / exclusive-window, not missing procedure.

---

## 4. Can Linux materialize `mix` without founder HITL?

| Path | Standing | Notes |
|------|----------|-------|
| Export off-PATH elixir-install `bin` onto PATH | **SUPPORTED** | Already on disk; no founder HITL |
| `curl` elixir-lang `install.sh` → pin 1.20.4-otp-29 | **SUPPORTED** | Egress allow-all; no founder secret |
| `sudo apt-get install elixir` (1.14) | **UNSUPPORTED** for this subject | Below `~> 1.17` |
| Founder HITL for `mix` binary alone | **Not required** | Distinct from foundry model-access / WebAuthn HITL |

Antifalsifier: materializing `mix` ≠ Jobs/C4 `ALIVE`. Crown still needs NIF + `--check` exit **0**.

---

## 5. Standing receipt

| Claim | Standing |
|-------|----------|
| Default PATH `mix` / `elixir` | **ABSENT** |
| Off-PATH elixir-install `mix` (1.19.0-otp-27) | **PRESENT** (materialized without HITL) |
| Pin `.tool-versions` 1.20.4-otp-29 | **ABSENT** (installable without HITL) |
| apt elixir 1.14 as subject toolchain | **UNSUPPORTED** |
| NIF build docs | **PRESENT** |
| `CXX=g++-13` exclusive NIF → `mix ggen_igniter.sync --check` | **BLOCKED** (prior `oxrocksdb-sys` SIGTERM; no thrash this land) |
| Hop-4 BrandTheme manufacture | **ALIVE** (unchanged; see [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md)) |
| Jobs/C4 / mix sync residual crown | **BLOCKED** |

---

## 6. Exact next verifier

Quiet exclusive pod window (no concurrent oxrocksdb / ui_layout cold builds), then:

```bash
export PATH="$HOME/.elixir-install/installs/elixir/1.19.0-otp-27/bin:$HOME/.elixir-install/installs/otp/27.1.2/bin:$PATH"
# optional pin: install.sh → elixir 1.20.4-otp-29 / erlang 29.1.2 per .tool-versions
cd /home/ubuntu/ggen_igniter   # @ b5a15528+
export CXX=g++-13
export CXXFLAGS="-I/usr/include/c++/13 -I/usr/include/x86_64-linux-gnu/c++/13"
export CARGO_TARGET_DIR=/tmp/igniter-nif-cargo-target-gxx13-exclusive
export CARGO_BUILD_JOBS=1
mix deps.get
mix compile
mix ggen_igniter.sync --check
# expect: exit 0 for residual ALIVE
```

Do **not** invent `mix sync` `ALIVE` while NIF remains **BLOCKED**.
Do **not** thrash cargo alongside other oxrocksdb consumers on the shared pod.

## Replay (honesty only)

```bash
command -v mix; echo mix_default_exit:$?
test -x "$HOME/.elixir-install/installs/elixir/1.19.0-otp-27/bin/mix"
test -x /usr/bin/g++-13
test -f docs/rfc/v27.1.1/HOP4-MIX-NIF-STANDING.md
```
