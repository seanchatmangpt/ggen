# IGNITER-CLEANROOM-STANDING — ggen_igniter clean-room (`git diff --exit-code == 0`)

| Field | Value |
|-------|-------|
| Document ID | `IGNITER-CLEANROOM-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (sibling WIP / Gate adjacency) |
| Subject | `seanchatmangpt/ggen_igniter` |
| Crown claim | Working tree clean-room iff `git diff --exit-code` exits **0** |
| Materialized | 2026-11-22 |
| Tip pin (this land) | tip-wins absorb onto crown pre-absorb `94ea8cf252f3a238ef89e7012f4c34d0ffbd98f2` (⊇ perimeter `94ea8cf25` + HITL §8 `8629691f8`); sidebranch evidence tip `893070836` / `bc-17cf3ea2`; prior tip-wins absorb `d0fec2f2f` (`bc-fd60df35`); prior ALIVE evidence tip `f1492ed5563b29c7065d188bceac6156c3397ce2` |
| Land branch | tip-wins absorb `cursor/v2711-igniter-cleanroom-retip-bc54` @ `5c6eae752` onto crown via `cursor/v2711-igniter-cleanroom-absorb-a038`; prior absorb `cursor/v2711-igniter-reconfirm-bc54` @ `ec079cc16` onto `d0fec2f2f`; prior land `cursor/v2711-igniter-cleanroom-alive-bc54` @ `49e986ee7` |
| Evidence | `/tmp/cursor/igniter-cleanroom-retip.md` (`bc-17cf3ea2`) — mount `git diff --exit-code` → **0**; absorb reconfirm EXIT **0** (`bc-c717fdd5`); prior `/tmp/cursor/igniter-cleanroom-reconfirm.md` (`bc-fd60df35`); prior `/tmp/cursor/igniter-cleanroom-alive-tip.md` (`bc-53b0e099`) |
| Repo scope of this land | `seanchatmangpt/ggen` (typed contract + standing only; tip-wins crown absorb; path crown remains **PARTIAL**; no ManagePullRequest #849) |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

Source presence, historical lane prose, and workflow definitions are **never** promoted to `ALIVE`.

---

## 1. Crown claim

```text
ALIVE(ggen_igniter clean-room)
  ⇔  observed: in checkout seanchatmangpt/ggen_igniter,
               `git diff --exit-code` → exit 0
```

Anything else is fail-closed:

| Observation | Standing | Typed refusal (in-repo) |
|-------------|----------|-------------------------|
| Sibling not mounted / not probed | **BLOCKED** | `REFUSED_CLEANROOM_CHECKOUT_UNMOUNTED` |
| Probe exit ≠ 0 | **REFUSED_** (dirty) | `REFUSED_CLEANROOM_DIRTY_TREE` |
| Probe on wrong subject | **UNKNOWN** | `REFUSED_CLEANROOM_SUBJECT_MISMATCH` |
| Probe exit 0 on admitted subject | **ALIVE** (that observation only) | — |

Scope note: `git diff --exit-code` covers **tracked unstaged** drift. Staged and untracked dirt are outside this crown string; operators may additionally run `git diff --cached --exit-code` and `git status --porcelain` before release, but those are **not** this claim.

**Distinction (hard):** clean-room crown `ALIVE` (this document) is **not** BrandTheme manufacture `ALIVE`, is **not** photolithography path crown `ALIVE`, and is **not** Foundry `--live` `ALIVE` (`BLOCKED_HITL` / `foundry_exit=1` remains open elsewhere).

---

## 2. Evidence searched in this workspace (2026-11-22 retip)

| Source | Finding | Standing contribution |
|--------|---------|------------------------|
| `/home/ubuntu/ggen_igniter` @ `b5a15528eb6792dd366fe744ca87f24a80cc46fc` | Mount **PRESENT**; `git diff --exit-code` → **0**; porcelain **0**; cached diff **0** | Clean-room crown **ALIVE** (tip-wins absorb onto pre-absorb `94ea8cf25` / side `5c6eae752` / `bc-17cf3ea2`; prior `d0fec2f2f` / `bc-fd60df35`) |
| `/agent/repos/ggen_igniter` | Same HEAD; `git diff --exit-code` → **0** | Mount parity **ALIVE** |
| Local path `/Users/sac/ggen_igniter` / `~/ggen_igniter` | **Absent** in this cloud workspace | Host-path variant **UNSUPPORTED** here (cloud mount used) |
| `docs/jira/v26.11.4/_LANES.md` IG-1 | Historical: “WIP-finish ~98 dirty files” | Stale dirty signal — **not** a live probe; do not treat as ALIVE or as current REFUSED |
| `crates/ggen-sjira/**` | Native SemanticJira port; references igniter Elixir SoT | Port standing ≠ clean-room standing |
| Remote `seanchatmangpt/ggen_igniter` (GitHub default branch) | Repo exists; HEAD tree is the published tip, not a local dirty index | Remote tip ≠ working-tree clean-room |
| GALL `gall_observe_clean_room` / `clean_room_rebuild.json` | In-repo **ggen** rebuild isolation, unrelated subject | Do not conflate |
| Warm NIF `/tmp/igniter-nif-cargo-target-gxx13-nif2` | Not required for clean-room crown string this retip | NIF residual owned elsewhere — **not** this crown; absence does not demote clean-room EXIT:0 |

---

## 3. In-repo court (contract only)

IO-free typed interface lives at:

- `crates/ggen-sjira/src/igniter_cleanroom.rs`
- Court: `crates/ggen-sjira/tests/igniter_cleanroom_court.rs`

| Claim | Standing | Verifier |
|-------|----------|----------|
| Clean-room **contract** (subject pin, verifier string, typed refusals, exit-0 admit shape) | **PARTIAL_ALIVE** | `cargo test -p ggen-sjira --test igniter_cleanroom_court` |
| Clean-room **of the sibling working tree** | **ALIVE** @ tip `893070836` (retip; prior `d0fec2f2f` / `f1492ed55`) — mount `/home/ubuntu/ggen_igniter` @ `b5a15528…`; `git diff --exit-code` → **0** | See §4 replay |

The court deliberately does **not** shell out to git and does **not** require the Elixir/mix binary. It encodes the contract so ambient narrative cannot mint `ALIVE`.

---

## 4. Replay verifier (crown met)

Crown string **ALIVE** reconfirmed 2026-11-22 on exact tip
`893070836` (`EXIT:0`; prior observation tips `d0fec2f2f` / `f1492ed55`).
Replay:

```bash
cd /home/ubuntu/ggen_igniter   # @ b5a15528eb6792dd366fe744ca87f24a80cc46fc
git diff --exit-code
# expected: exit 0 for ALIVE; nonzero → REFUSED_CLEANROOM_DIRTY_TREE
# retip agent bc-17cf3ea2 → EXIT:0 (/tmp/cursor/igniter-cleanroom-retip.md)
```

Optional fuller hygiene (not this crown string):

```bash
git diff --cached --exit-code
test -z "$(git status --porcelain)"
```

Replay the in-repo contract court anytime:

```bash
cargo test -p ggen-sjira --test igniter_cleanroom_court
```

---

## 5. Standing receipt (this land)

| Claim | Standing |
|-------|----------|
| Contract module + court in ggen | **PARTIAL_ALIVE** |
| Sibling clean-room (`git diff --exit-code == 0`) | **ALIVE** EXIT:**0** @ tip `893070836` (`bc-17cf3ea2` / `/tmp/cursor/igniter-cleanroom-retip.md`); prior tip lineage `d0fec2f2f` / `bc-fd60df35` + `f1492ed55` / `bc-53b0e099`. Mount **PRESENT** `/home/ubuntu/ggen_igniter` @ `b5a15528…`. Clean-room crown ≠ BrandTheme manufacture `ALIVE` (already separate) ≠ photolithography path crown ≠ Foundry `--live` / zero-HITL crown |
| Tip BrandTheme manufacture | **ALIVE** — tip dual-schema `fourth-watch.brand.toml` (`[rhythm]` + DTCG `[primitive.space]`) BrandTheme emit vs igniter fixture → `diff -rq` **0** + compare-subject `git diff --exit-code` → **0** (`/tmp/cursor/igniter-real-cleanroom.md`; reconfirm `cursor/v2711-hop4-primitive-space-bc54`). Rhythm-only probe still `REFUSED :no_primitive_space` (antifalsifier). |
| Foundry `--live` | **BLOCKED_HITL** / **REFUSED** (`foundry_exit=1`) — **not** promoted by this clean-room retip |
| Historical IG-1 dirty census as current truth | **UNSUPPORTED** as live evidence (stale lane prose) |
| OOR | **BLOCKED** (unchanged; parent owns #849 body; OOR docs exclusive elsewhere) |

### 5b. Manufacture next verifier (hop-4 BrandTheme)

**Closed for BrandTheme compare:** tip dual-schema fixture admits; replay:

```bash
# BrandTheme emit tip docs/rfc/v27.1.1/fixtures/fourth-watch.brand.toml
# + igniter test/fixtures/brand_theme/fourth-watch.brand.toml (canon path)
diff -rq from_tip.css from_igniter.css   # expect 0
git diff --exit-code                     # compare subject → 0
```

**Residual (not hop-4 BrandTheme crown):** Jobs/C4 `mix ggen_igniter.sync
--check` → **ALIVE** (admitted `--pack-dir test/fixtures/sample-pack … --check`
exit **0**; evidence `/tmp/cursor/hop4-mix-check-alive-probe.md`). Probe ledger:
[`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md) — default PATH `mix`
**ABSENT**; off-PATH elixir-install **PRESENT** (exported); `CXX=g++-13` NIF
**ALIVE**; bare `--check` **REFUSED_INVOCATION** (exit 2 antifalsifier;
`--template`/`--pack` required). Do **not** invent Jobs/C4 `ALIVE` from NIF
`.so` alone or from bare exit-2. Antifalsifier: bare post-clone
`git diff --exit-code` ≠ hop-4 manufacture `ALIVE`; off-PATH `mix` PRESENT ≠
Jobs/C4 `ALIVE`.

**Tip-wins:** Jobs/C4 residual already **ALIVE** on crown; this clean-room retip
does **not** reopen or thrash mix/`write_apply` siblings. Replay admitted
pack/template `--check` → **0**.

Do not merge this document’s prose into a Plane-5 / release `ALIVE` without
replaying §4 (clean-tree) **and** §5b BrandTheme compare on the admitted tip.

---

## 6. Tip-wins absorb receipt (2026-11-22 — pre-absorb `94ea8cf25`)

| Field | Value |
|-------|-------|
| Agent (retip) | `bc-17cf3ea2-9f0b-5c30-9ec6-7ab3310e6aa1` |
| Agent (absorb) | `bc-c717fdd5-a98a-559a-91aa-12bfd575a038` |
| Tip | pre-absorb `94ea8cf252f3a238ef89e7012f4c34d0ffbd98f2` (`origin/cursor/ecosystem-spec-27-1-1-bc54`; ⊇ perimeter + HITL §8; sidebranch evidence tip `893070836`) |
| Branch | tip-wins absorb `cursor/v2711-igniter-cleanroom-retip-bc54` @ `5c6eae752` via `cursor/v2711-igniter-cleanroom-absorb-a038` |
| Command | `cd /home/ubuntu/ggen_igniter && git diff --exit-code` |
| EXIT | **0** (retip + absorb reconfirm) |
| Claim holds | `git diff --exit-code == 0` **yes** |
| Standing | Clean-room crown **ALIVE** (reconfirmed); photolithography path crown **PARTIAL** (unchanged — **not** promoted); Foundry `--live` **BLOCKED_HITL** (unchanged — clean-room ≠ `--live` ALIVE) |
| Exact next verifier | BrandTheme manufacture compare replay → `diff -rq`/`git diff --exit-code` **0**; or Jobs/C4 admitted `mix ggen_igniter.sync --pack-dir test/fixtures/sample-pack … --check` → **0** (default PATH `mix` **ABSENT** this env); path crown next open: `bun scripts/powl-foundry-loop.ts --live` → **0** (**BLOCKED_HITL**) |
