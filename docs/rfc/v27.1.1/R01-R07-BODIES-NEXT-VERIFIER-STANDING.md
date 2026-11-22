# R-01…R-07 risk-register bodies — exact next verifier pin (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `R01-R07-BODIES-NEXT-VERIFIER-STANDING-27.1.1` |
| Branch | tip-wins absorb of `cursor/v2711-brce-bodies-next-bc54` @ `4f9a3685e` (bodies pin `6e227c187`; prior absorb `ba5d26903`/`21e4cb941` content-≡) onto crown via `cursor/v2711-timer-settle-bb41` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §11 / D7; OOR **G3-5** / BRCE **V10** |
| Companions | [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2; [`R01-R07-TIP-CARGO-STANDING.md`](./R01-R07-TIP-CARGO-STANDING.md); [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md); [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 **V10**; [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) §2; [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) **G3-5** |
| Observed ggen tip (side base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `bebea5dd0d4c9af533457e04e0ed068ae8844f69` (⊇ HITL retip2 **BLOCKED_HITL** + zcode refusal **ALIVE** + OOR retip14) |
| Probe id | `bc-6fa5335f` |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty pin) |
| Status | LEDGER — all seven *bodies* **BLOCKED** (sovereign risk-register unmounted); resolution *contract* tip-cargo **ALIVE** retained (**7** @ `e7569893a`); **no** body / V8 / live-ceremony `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** This document pins the **exact Founder HITL next verifier** for
R-01…R-07 *body* admission (OOR **G3-5** / RC-D7) so crown can cite one closed
ladder. It does **not** invent risk prose, promote the in-repo contract fence to
body `ALIVE`/`CLOSED`, or claim BRCE **V8** / live WebAuthn / Plane-5 `ALIVE`.

---

## 1. Findings (read-only census)

| Subject | Location / evidence | Standing |
|---------|---------------------|----------|
| §11 enumeration R-01…R-07 | `ECOSYSTEM-SPEC-27.1.1.md` §11 | Spec text **PARTIAL_ALIVE** |
| Resolution *contract* fence | `ggen_graph::ecosystem_risk` + `r01_r07_resolution_court` | **ALIVE** (exact tip-cargo **7** @ `e7569893a`) |
| Body standing API | `current_body_standing` → `RiskBodyStanding::Blocked` for every R-* | **BLOCKED** (all seven) |
| Ungrounded “closed” | `resolve_closure(..., risk_body_admitted=false)` → `REFUSED_UNGROUNDED` | Refuse path **ALIVE** (contract only) |
| Sovereign risk-register mount | Not present under `docs/rfc/v27.1.1/`, `/agent/repos/{ggen,ggen_igniter,zcode-cli}`, or admitted sibling path | **UNSUPPORTED** / bodies **BLOCKED** |
| Legacy archive `12_RISK_REGISTER.md` | `docs/archive/**` — **RSK-001…** vocabulary, not R-01…R-07 | **REFUSED_UNGROUNDED** as §11 body source |
| BRCE **V8** broker | Unmounted; [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) | **BLOCKED** (orthogonal owner **BRCE**; ≠ body closure) |

**Decision (narrow court vs docs pin).** A new in-repo court/fixture that
“admits” bodies would require invented R-* prose or a non-admitted archive
transcript → `REFUSED_UNGROUNDED`. The existing Chicago court already seals the
typed refuse path for ungrounded closure. Therefore this land is **docs-only**:
bodies stay **BLOCKED** with the §3 exact next command. **No** cargo court added.

**V8 independence.** Body closure does **not** require a mounted V8 broker
runtime (owner = risk-register + Founder HITL; OOR **G3-5**). V8 remains a
separate **BLOCKED** subject (OOR **G2-3**). Absence of V8 is **not** the body
blocker — absence of the admitted sovereign risk-register is. Neither subject
may be promoted to `ALIVE` from this pin.

---

## 2. Standing pin (honesty)

| Claim | Standing | Exact reason (held; not invented) |
|-------|----------|-----------------------------------|
| R-01…R-07 *bodies* (per-ID closure) | **BLOCKED** (all seven) | Sovereign risk-register bodies unmounted ([`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2; OOR **G3-5**) |
| R-01…R-07 *contract* fence | tip-cargo **ALIVE** (**7** @ `e7569893a`) | Contract ≠ body closure; replay alone is antifalsifier for body `ALIVE` |
| Invented / archive-transcribed R-* prose as body | **REFUSED_UNGROUNDED** | ECOSYSTEM-SPEC §11 + standing law |
| Silent “closed” without admitted body | **REFUSED_UNGROUNDED** | `resolve_closure` + court |
| BRCE **V8** broker / live ceremony | **BLOCKED** / **UNSUPPORTED** | Separate pin — do **not** claim from this land |
| Plane-5 **V9** / body **V10** GA close | **BLOCKED** | After register admit + per-ID typed receipt/`REFUSED_*` |

---

## 3. Owner / HITL

| Role | Owner |
|------|-------|
| Exact next body converge | **Founder HITL** — admit sovereign risk-register path (agent must **not** invent prose) |
| Execution surface | Multi-repo **risk-register** owner (out of ggen DO) |
| In-repo contract fence (non-substitute) | **ggen** — `r01_r07_resolution_court` only |
| OOR row | **G3-5** / RC-D7 ([`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md)) |
| BRCE ladder cite | **V10** ([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5) |
| Slice ledgers | [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2; [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) §2 |

---

## 4. Exact next verifier (unstick bodies)

**Falsifier for R-01…R-07 body closure / G3-5:** Founder HITL must admit the
sovereign risk-register, materialize each R-* body under that path, then file a
typed receipt **or** typed `REFUSED_*` per ID before **2027-01-01**. Until then
standing stays **BLOCKED** — **not** `ALIVE` / `CLOSED`.

```bash
# Founder HITL — R-01…R-07 body admission (OOR G3-5 / RC-D7 / V10)
# Owner: risk-register (multi-repo) + Founder HITL. Agent must not invent prose.
#
# Preconditions (admit — do not invent in ggen):
#   1) Founder HITL names/mounts the sovereign risk-register path that owns
#      R-01…R-07 body text (not docs/archive/**/12_RISK_REGISTER.md;
#      not invented fixtures; not V8 broker presence alone).
#   2) Materialize each R-01…R-07 body under that admitted path.
#
# Exact falsifier (per ID R-01…R-07):
#   A) Body ABSENT / unadmitted → keep BLOCKED; closed claim → REFUSED_UNGROUNDED
#   B) Body ADMITTED → file typed receipt OR typed REFUSED_* before 2027-01-01
# Pass: all seven IDs have typed receipt or typed REFUSED_*; no silent close.
# Until then: BLOCKED — sovereign risk-register bodies not mounted.
#
# Antifalsifiers (do NOT promote from these):
#   - cargo test -p ggen-graph --test r01_r07_resolution_court   # contract ≠ body ALIVE
#   - cargo test -p ggen-abb-sbb --test brce_as01_seat_court     # V1 fence ≠ body / V8
#   - cargo test -p ggen-abb-sbb --test brce_as01_es256_assertion_court  # V7 ≠ body
#   - BRCE V8 broker mount / live WebAuthn ceremony               # orthogonal; ≠ body close
#   - Transcribing docs/archive/**/12_RISK_REGISTER.md (RSK-*)    # REFUSED_UNGROUNDED
#   - Claiming body ALIVE/CLOSED from this NEXT-VERIFIER doc      # REFUSED_UNGROUNDED
#
# In-repo contract replay only (non-substitute; bodies remain BLOCKED):
CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test r01_r07_resolution_court \
  --locked --config build.jobs=1 -- --test-threads=1
```

Owner artifact wording: [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) §2.
V8 residual (orthogonal): [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md) §3.

---

## 5. Explicit non-claims

- This sidebranch does **not** claim R-01…R-07 body `ALIVE` or `CLOSED`.
- This sidebranch does **not** claim BRCE **V8** `ALIVE`, live WebAuthn `ALIVE`,
  or Plane-5 **V9** `ALIVE`.
- Contract tip-cargo **ALIVE** (**7** @ `e7569893a`) does **not** close **G3-5**.
- No risk-register file was invented, transcribed from memory, or promoted from
  archive `RSK-*` vocabulary.
- No new Chicago court or fixture body was added (would be ungrounded without
  Founder HITL register admit).
- Retain **BLOCKED** with this pin until §4 per-ID typed receipt/`REFUSED_*`
  lands — docs pin ≠ body `ALIVE`.

---

## 6. Provenance

| Event | Note |
|-------|------|
| Side-branch pin | 2026-11-22 — `cursor/v2711-brce-bodies-next-bc54` @ `21e4cb941` from crown tip `bebea5dd0` (`bc-6fa5335f`); docs-only; bodies **BLOCKED**; contract **ALIVE** retained; **no** V8/body `ALIVE`; no cargo; no ManagePullRequest; **no** crown push |
| Content tip re-pin | 2026-11-22 — sidebranch tip `4f9a3685e` / bodies pin `6e227c187` (content-≡ prior `ba5d26903`/`21e4cb941`); bodies remain **BLOCKED** |
| Tip-wins absorb (timer settle) | 2026-11-22 — `cursor/v2711-timer-settle-bb41` tip-wins cites `4f9a3685e` onto crown tip `a85bebfb2` (⊇ webauthn-retip2 **UNSUPPORTED** + OOR retip15 + curve + HITL/zcode); standing **BLOCKED** held; **no** body/`ALIVE`; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Content tip re-pin | 2026-11-22 — sidebranch tip `4f9a3685e` / bodies pin `6e227c187` (file blob content-≡ prior `ba5d26903`/`21e4cb941`); bodies remain **BLOCKED** |
| Tip-wins absorb (content tip) | 2026-11-22 — tip-wins absorb `cursor/v2711-brce-bodies-next-bc54` @ `4f9a3685e` onto crown tip `a85bebfb2` (⊇ webauthn-retip2 **UNSUPPORTED** @ `7ac7fee56` content) via `cursor/v2711-webauthn-bodies-absorb-edb4`; bodies **BLOCKED** held; **no** invented body/V8 `ALIVE`; docs-only; no cargo; no ManagePullRequest; push `HEAD:cursor/ecosystem-spec-27-1-1-bc54` (no force) |
| Prior evidence chain | `r01_r07_resolution_court` tip-cargo **ALIVE** @ `e7569893a` → R01-R07 standing bodies **BLOCKED** → BRCE-V8-NEXT bodies **BLOCKED** ahead of / parallel to V8 → OOR **G3-5** still open → this NEXT-VERIFIER honesty pin |
| Replay | `test -f docs/rfc/v27.1.1/R01-R07-BODIES-NEXT-VERIFIER-STANDING.md` && `rg -n 'BLOCKED|REFUSED_UNGROUNDED|G3-5' docs/rfc/v27.1.1/R01-R07-BODIES-NEXT-VERIFIER-STANDING.md` |
