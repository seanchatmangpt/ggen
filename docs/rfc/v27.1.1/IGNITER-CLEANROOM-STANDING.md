# IGNITER-CLEANROOM-STANDING — ggen_igniter clean-room (`git diff --exit-code == 0`)

| Field | Value |
|-------|-------|
| Document ID | `IGNITER-CLEANROOM-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (sibling WIP / Gate adjacency) |
| Subject | `seanchatmangpt/ggen_igniter` |
| Crown claim | Working tree clean-room iff `git diff --exit-code` exits **0** |
| Materialized | 2026-11-22 |
| Tip pin (this land) | evidence tip `f1492ed5563b29c7065d188bceac6156c3397ce2`; absorb onto crown `42717a7a0c8e2a67cdbeebdab259480eaf90138c` (post Jobs/C4 ALIVE @ `97c613452`) |
| Land branch | `cursor/v2711-igniter-cleanroom-alive-bc54` @ `49e986ee7` |
| Evidence | `/tmp/cursor/igniter-cleanroom-alive-tip.md` (`bc-53b0e099`) — mount `git diff --exit-code` → **0** |
| Repo scope of this land | `seanchatmangpt/ggen` (typed contract + standing only) |

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

---

## 2. Evidence searched in this workspace (2026-11-22)

| Source | Finding | Standing contribution |
|--------|---------|------------------------|
| `/home/ubuntu/ggen_igniter` @ `b5a15528eb6792dd366fe744ca87f24a80cc46fc` | Mount **PRESENT**; `git diff --exit-code` → **0**; porcelain **0**; cached diff **0** | Clean-room crown **ALIVE** (tip `f1492ed55`) |
| `/agent/repos/ggen_igniter` | Same HEAD; `git diff --exit-code` → **0** | Mount parity **ALIVE** |
| Local path `/Users/sac/ggen_igniter` / `~/ggen_igniter` | **Absent** in this cloud workspace | Host-path variant **UNSUPPORTED** here (cloud mount used) |
| `docs/jira/v26.11.4/_LANES.md` IG-1 | Historical: “WIP-finish ~98 dirty files” | Stale dirty signal — **not** a live probe; do not treat as ALIVE or as current REFUSED |
| `crates/ggen-sjira/**` | Native SemanticJira port; references igniter Elixir SoT | Port standing ≠ clean-room standing |
| Remote `seanchatmangpt/ggen_igniter` (GitHub default branch) | Repo exists; HEAD tree is the published tip, not a local dirty index | Remote tip ≠ working-tree clean-room |
| GALL `gall_observe_clean_room` / `clean_room_rebuild.json` | In-repo **ggen** rebuild isolation, unrelated subject | Do not conflate |
| Warm NIF `/tmp/igniter-nif-cargo-target-gxx13-nif2` | `libggen_graph_nif.so` **PRESENT**; no rebuild this land | NIF residual owned elsewhere — not this crown |

---

## 3. In-repo court (contract only)

IO-free typed interface lives at:

- `crates/ggen-sjira/src/igniter_cleanroom.rs`
- Court: `crates/ggen-sjira/tests/igniter_cleanroom_court.rs`

| Claim | Standing | Verifier |
|-------|----------|----------|
| Clean-room **contract** (subject pin, verifier string, typed refusals, exit-0 admit shape) | **PARTIAL_ALIVE** | `cargo test -p ggen-sjira --test igniter_cleanroom_court` |
| Clean-room **of the sibling working tree** | **ALIVE** @ tip `f1492ed55` — mount `/home/ubuntu/ggen_igniter` @ `b5a15528…`; `git diff --exit-code` → **0** | See §4 replay |

The court deliberately does **not** shell out to git and does **not** require the Elixir/mix binary. It encodes the contract so ambient narrative cannot mint `ALIVE`.

---

## 4. Replay verifier (crown met)

Crown string **ALIVE** observed 2026-11-22 on tip `f1492ed55` (`EXIT:0`). Replay:

```bash
cd /home/ubuntu/ggen_igniter   # @ b5a15528eb6792dd366fe744ca87f24a80cc46fc
git diff --exit-code
# expected: exit 0 for ALIVE; nonzero → REFUSED_CLEANROOM_DIRTY_TREE
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
<<<<<<< HEAD
| Sibling clean-room (`git diff --exit-code == 0`) | **ALIVE** EXIT:**0** @ tip lineage `f1492ed55` — mount **PRESENT** `/home/ubuntu/ggen_igniter` @ `b5a15528…`; agent `bc-53b0e099` (`/tmp/cursor/igniter-cleanroom-alive-tip.md`). Clean-room crown ≠ BrandTheme manufacture `ALIVE` (already separate) ≠ Foundry zero-HITL crown |
=======
| Sibling clean-room (`git diff --exit-code == 0`) | **ALIVE** — tip `f1492ed5563b29c7065d188bceac6156c3397ce2`; mount `/home/ubuntu/ggen_igniter` @ `b5a15528…`; `EXIT:0` (`/tmp/cursor/igniter-cleanroom-alive-tip.md`; `bc-53b0e099`). Antifalsifier: this crown ≠ BrandTheme manufacture ALIVE (already separate). |
>>>>>>> aa8079108 (docs(rfc): tip-wins absorb igniter clean-room ALIVE onto crown)
| Tip BrandTheme manufacture | **ALIVE** — tip dual-schema `fourth-watch.brand.toml` (`[rhythm]` + DTCG `[primitive.space]`) BrandTheme emit vs igniter fixture → `diff -rq` **0** + compare-subject `git diff --exit-code` → **0** (`/tmp/cursor/igniter-real-cleanroom.md`; reconfirm `cursor/v2711-hop4-primitive-space-bc54`). Rhythm-only probe still `REFUSED :no_primitive_space` (antifalsifier). |
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

**Tip-wins:** Jobs/C4 residual already **ALIVE** on crown (`97c613452` / `/tmp/cursor/hop4-mix-check-alive-probe.md`); this clean-room land does **not** reopen or thrash mix/`write_apply` siblings. Replay admitted pack/template `--check` → **0**. Warm NIF target reused (no oxrocksdb).

Do not merge this document’s prose into a Plane-5 / release `ALIVE` without
replaying §4 (clean-tree) **and** §5b BrandTheme compare on the admitted tip.
