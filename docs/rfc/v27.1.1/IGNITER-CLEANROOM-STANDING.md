# IGNITER-CLEANROOM-STANDING — ggen_igniter clean-room (`git diff --exit-code == 0`)

| Field | Value |
|-------|-------|
| Document ID | `IGNITER-CLEANROOM-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (sibling WIP / Gate adjacency) |
| Subject | `seanchatmangpt/ggen_igniter` |
| Crown claim | Working tree clean-room iff `git diff --exit-code` exits **0** |
| Materialized | 2026-11-22 |
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
| Local path `/Users/sac/ggen_igniter` / `~/ggen_igniter` | **Absent** in this cloud workspace | Checkout **BLOCKED** / unmounted |
| `docs/jira/v26.11.4/_LANES.md` IG-1 | Historical: “WIP-finish ~98 dirty files” | Stale dirty signal — **not** a live probe; do not treat as ALIVE or as current REFUSED |
| `crates/ggen-sjira/**` | Native SemanticJira port; references igniter Elixir SoT | Port standing ≠ clean-room standing |
| Remote `seanchatmangpt/ggen_igniter` (GitHub default branch) | Repo exists; HEAD tree is the published tip, not a local dirty index | Remote tip ≠ working-tree clean-room |
| GALL `gall_observe_clean_room` / `clean_room_rebuild.json` | In-repo **ggen** rebuild isolation, unrelated subject | Do not conflate |

---

## 3. In-repo court (contract only)

IO-free typed interface lives at:

- `crates/ggen-sjira/src/igniter_cleanroom.rs`
- Court: `crates/ggen-sjira/tests/igniter_cleanroom_court.rs`

| Claim | Standing | Verifier |
|-------|----------|----------|
| Clean-room **contract** (subject pin, verifier string, typed refusals, exit-0 admit shape) | **PARTIAL_ALIVE** | `cargo test -p ggen-sjira --test igniter_cleanroom_court` |
| Clean-room **of the sibling working tree** | **BLOCKED** (checkout unmounted here) | See §4 |

The court deliberately does **not** shell out to git and does **not** require the Elixir/mix binary. It encodes the contract so ambient narrative cannot mint `ALIVE`.

---

## 4. Next exact verifier (crown ALIVE)

Materialize `seanchatmangpt/ggen_igniter`, then:

```bash
cd /path/to/ggen_igniter
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
| Sibling clean-room (`git diff --exit-code == 0`) | **PARTIAL** checkpoint — mount **PRESENT** `/home/ubuntu/ggen_igniter` @ `b5a15528…`; clean-tree exit **0** (≠ manufacture ALIVE) |
| Tip BrandTheme manufacture | **ALIVE** — tip dual-schema `fourth-watch.brand.toml` (`[rhythm]` + DTCG `[primitive.space]`) BrandTheme emit vs igniter fixture → `diff -rq` **0** + compare-subject `git diff --exit-code` → **0** (`/tmp/cursor/igniter-real-cleanroom.md`; reconfirm `cursor/v2711-hop4-primitive-space-bc54`). Rhythm-only probe still `REFUSED :no_primitive_space` (antifalsifier). |
| Historical IG-1 dirty census as current truth | **UNSUPPORTED** as live evidence (stale lane prose) |

### 5b. Manufacture next verifier (hop-4 crown)

**Closed for BrandTheme compare:** tip dual-schema fixture admits; replay:

```bash
# BrandTheme emit tip docs/rfc/v27.1.1/fixtures/fourth-watch.brand.toml
# + igniter test/fixtures/brand_theme/fourth-watch.brand.toml (canon path)
diff -rq from_tip.css from_igniter.css   # expect 0
git diff --exit-code                     # compare subject → 0
```

**Residual (not hop-4 BrandTheme crown):** exclusive `CXX=g++-13` NIF →
`mix ggen_igniter.sync --check` → 0. Do **not** invent `mix sync` `ALIVE`
while NIF **BLOCKED**. Antifalsifier: bare post-clone `git diff --exit-code` ≠
hop-4 manufacture `ALIVE`.

Do not merge this document’s prose into a Plane-5 / release `ALIVE` without
executing §4 (clean-tree) **and** replaying §5b BrandTheme compare on the
admitted tip.
