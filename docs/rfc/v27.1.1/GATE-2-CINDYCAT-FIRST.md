# Gate 2 — CindyCat-first commercial priority (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `GATE-2-CINDYCAT-FIRST` |
| Companion | [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md), [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) |
| Deal doctrine | [`CINDYCAT-REVERSE-GLOO.md`](./CINDYCAT-REVERSE-GLOO.md), [`LINKEDIN-DEAL-PLAYBOOK.md`](./LINKEDIN-DEAL-PLAYBOOK.md) |
| Brand fixtures | [`fixtures/cindycat.brand.toml`](./fixtures/cindycat.brand.toml), [`fixtures/fourth-watch.brand.toml`](./fixtures/fourth-watch.brand.toml) |
| Spine | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md), [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md), [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) |
| Status | Priority ledger — **not** Gate-1 rewrite; **not** deal `ALIVE` |
| Observed | 2026-11-22 |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

---

## 1. Priority rule (Gate 2 commercial)

**CindyCat patronage is FIRST.** Fourth Watch cold cyber is **not** the Gate 2 commercial open.

| Rank | Motion | Why |
|------|--------|-----|
| **1** | CindyCat patronage (Reverse Gloo; Event #1 non-negotiables) | Warm Jobs face; 0% custody / 0.00% Neshek; Julia Veach brand language |
| **2** | LinkedIn **direct outreach** (DM scripts) | Named conversations before broadcast |
| **3** | Content calendar (Tue/Wed/Thu) | Supports outreach; never replaces it |
| **4** | Fourth Watch | Natural **follow-on** on an existing CindyCat-trusted relationship — refuse cold cyber open |

ECOSYSTEM Gate 2–3 still lists revops, brand `.toml` inventories, CASTLE publish, and release courts as technical work. This ledger **narrows commercial priority** without claiming those technical rows `ALIVE`.

---

## 2. Relationship to Gate 1

[`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md) remains the requirement → evidence → verifier → owner matrix for Dec 6–18 in-repo fences.

| Gate 1 row (relevant) | Standing | Gate 2 implication |
|-----------------------|----------|--------------------|
| Brand `.toml` rhythm + OEM stubs (Fourth Watch / CindyCat) | **PARTIAL_ALIVE** (rhythm court) | Full DTCG color/type/CASTLE inventory still Gate 2+ — do not invent tokens to “look commercial” |
| Plane 5 front doors | **UNKNOWN** / out of DO | Commercial CindyCat-first does **not** promote Plane-5 `ALIVE` from ggen |

Gate 1 green courts do **not** satisfy Event #1 (zero custody, third-party payer, >$250 acknowledgment).

---

## 3. Commercial gap matrix (Gate 2)

| Requirement | Current evidence | Next exact verifier | Owner |
|-------------|------------------|---------------------|-------|
| Reverse Gloo axis + fee law documented | [`CINDYCAT-REVERSE-GLOO.md`](./CINDYCAT-REVERSE-GLOO.md) | Presence + human review of three non-negotiables | ops / brand (doctrine in ggen) |
| LinkedIn profile + DM scripts ready | [`LINKEDIN-DEAL-PLAYBOOK.md`](./LINKEDIN-DEAL-PLAYBOOK.md) | Operator paste + outbound DM log (external) | founder / brand |
| CindyCat before Fourth Watch in outreach | This doc §1 | Spot-check: no cold Fourth Watch first-touch in the week’s DM log | founder / brand |
| Event #1 with three non-negotiables | none in ggen — **UNKNOWN** | Written acknowledgment: 0% custody, named third-party payer, amount >$250 | ops / commercial |
| Close funnel #1/#2/#3 | playbook §4 bands only | Signed instrument per close (external SOW/register) | ops |
| CASTLE CindyCat public mint | **UNKNOWN** / sibling | castle publish with `--brand` CindyCat + RedactionCourt ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md)) | CASTLE plane |
| LinkedIn/Jobs automated send | **BLOCKED** | cos `/readyz` 200 ∧ 2× durable LOW | CASTLE + cos |
| Plane-5 CindyCat front door | **UNKNOWN** ([`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md)) | External hosting/AS-01 court — not claimable from ggen | Plane 5 |
| Full CindyCat DTCG inventory | rhythm stub only | Gate 2+ token inventory from admitted brand sources | brand + ggen schema |

Do not promote any row above to `ALIVE` from documentation presence alone.

---

## 4. Explicit refusals

| Anti-pattern | Standing |
|--------------|----------|
| Fourth Watch cold cyber as first LinkedIn touch | **REFUSED_COLD_FOURTH_WATCH** |
| Content calendar without DM wave | **REFUSED_CALENDAR_FIRST** (priority inversion) |
| Event #1 under $250, with custody, or without named payer | **REFUSED_EVENT_1** |
| Claiming deal or Plane-5 `ALIVE` from this RFC tree | **REFUSED_UNGROUNDED** |
| Inventing CASTLE color/type tokens in ggen to unblock sales | **REFUSED** (ECOSYSTEM non-goal) |

---

## 5. Next aggregate verifier

```text
# Doctrine presence (this land)
test -f docs/rfc/v27.1.1/GATE-2-CINDYCAT-FIRST.md
test -f docs/rfc/v27.1.1/CINDYCAT-REVERSE-GLOO.md
test -f docs/rfc/v27.1.1/LINKEDIN-DEAL-PLAYBOOK.md
rg -n 'CindyCat patronage is FIRST|REFUSED_COLD_FOURTH_WATCH' docs/rfc/v27.1.1/GATE-2-CINDYCAT-FIRST.md

# Gate-1 brand rhythm still green (does not prove commercial close)
cargo test -p ggen-graph --test brand_toml_rhythm_court
```

External (required for any commercial `ALIVE` claim): written Event #1 acknowledgment + ops register entry. Until then, commercial crown standing stays **UNKNOWN**.
