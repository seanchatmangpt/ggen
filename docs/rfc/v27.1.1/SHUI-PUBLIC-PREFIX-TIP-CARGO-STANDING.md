# shui_public_prefix_court tip-cargo — STALLED (exact tip) receipt (v27.1.1)

```text
cargo_tip=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
court=shui_public_prefix_court
package=ggen-graph
passed=UNKNOWN
failed=UNKNOWN
exit=137
standing=STALLED
```

**Exact-tip attempt (`cursor/v2711-shui-exact-tip-bc54`):** required verifier

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2 \
  cargo test -p ggen-graph --test shui_public_prefix_court --locked
```

on `origin/cursor/ecosystem-spec-27-1-1-bc54` tip ≥ `a85bebfb2` did **not** produce a trustworthy EXIT **0** with **4 passed**. Do **not** invent exact-tip `ALIVE`. Lineage tip-cargo **ALIVE** @ crown `008a81d21` (**4 passed**) retained — not a substitute for tip-subject fence `ALIVE`.

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-TIP-CARGO-STANDING` |
| Official evidence branch | `cursor/v2711-shui-exact-tip-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` Gate-1 `shui:` public-prefix fence |
| Attempt tip (final) | `cc2a4ea662b12b35ac90246b3a2bfbd89db615e4` (≥ `a85bebfb2`; ancestor OK) |
| Lineage evidence | `008a81d21` — **ALIVE (lineage tip-cargo)** **4 passed; exit 0** (`cursor/v2711-shui-prefix-alive-bc54`) |
| Target dir | `/tmp/ggen-shui-prefix-target2` (required) |
| Constraint | `CARGO_BUILD_JOBS=1`; `--locked` |
| Exact-tip standing | **STALLED** |
| Crown | **not touched** |
| ManagePullRequest | **not used** |

**Law:** Exact-tip `ALIVE` requires EXIT **0** + **4 passed** on the live crown tip for this court under the prescribed target. SIGKILL/SIGTERM / vacuous tee EXIT without a `test result:` line is **STALLED**, not `ALIVE`.

---

## Diagnostic (attempts)

| # | Mode | Result | Notes |
|---|------|--------|-------|
| 1 | `tee` without `pipefail` @ `a85bebfb2` | vacuous EXIT:0 | log truncated mid-compile (`petgraph`); **not** ALIVE |
| 2 | `pipefail` + prescribed target | **EXIT:137** | `Blocking waiting for file lock on build directory` ~9m then SIGKILL |
| 3 | `flock -x` wrapper | **EXIT:137** | wrapper SIGKILL within ~5s |
| 4 | quiet retry | **EXIT:137** | killed at `Compiling zerocopy` within ~4s |
| 5 | retry while tip3 exclusive elsewhere | **EXIT:137** | killed ~1s; `/tmp/gate1-exact-tip3/guardian.py` SIGKILL of non-`…/exclusive` cargos |
| 6 | post-guardian | **EXIT:143** | guardian cleared; SIGTERM at `Compiling zerocopy` / package-cache lock |

Logs: `/tmp/cursor/shui-exact-tip-mine.log`, `…-retry.log`, `…-last.log`, `…-postguard.log`, `/tmp/cursor/shui-exact-tip-stalled.status`.

Competing thrash: exclusive flock waiters; `gate1-exact-tip2` warm-reuse of `/tmp/ggen-shui-prefix-target2`; tip3 `guardian.py` killing foreign `cargo test`/`rustc`/`cc1plus`.

---

## Replay (exact; next operator)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
TIP=$(git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54)
git merge-base --is-ancestor a85bebfb2 "$TIP"
# ensure no foreign guardian: pgrep -af 'gate1-exact-tip.*/guardian'
set -o pipefail
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2 \
  cargo test -p ggen-graph --test shui_public_prefix_court --locked
# expect: test result: ok. 4 passed; EXIT:0 → promote STALLED → ALIVE (exact tip-cargo)
```

---

## Compact return line

```text
cargo_tip=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
EXIT=137
standing=STALLED
passed=UNKNOWN
landed=cursor/v2711-shui-exact-tip-bc54
next=quiet exclusive reconfirm on /tmp/ggen-shui-prefix-target2 → 4 passed EXIT:0
```
