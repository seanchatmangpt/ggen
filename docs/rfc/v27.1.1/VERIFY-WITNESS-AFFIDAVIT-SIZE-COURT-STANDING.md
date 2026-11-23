# `verify_witness` production WASM — affidavit size court (clause 3)

```text
exit=0 standing=ALIVE(affidavit_production_pin) bytes=28673 pin=563eb322a
absorb_base=1dbf1cc2e5a779cbaaaae6ad2075809e5c53be79
side=cursor/v2711-vw-affidavit-court-bc54@cc0ea07e4
probe=bc-5f5786ff-6189-5989-82ba-0a4087074e65
ggen_wire=UNSUPPORTED
honesty=affidavit_ALIVE_neq_ggen_production_ALIVE
```

| Field | Value |
|-------|-------|
| Subject | `seanchatmangpt/affidavit` @ `563eb322a16d805ebc99b2c16f1165462244d7e2` |
| Tip vs pin | `origin/main` ≡ pin (`0/0` left-right); tip does **not** differ |
| Verifier | `just gatekeeper-wasm-size-court` → `cd affidavit-gatekeeper-wasm && cargo test --locked --test size_court -- --nocapture` |
| EXIT | **0** |
| Artifact | `affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm` |
| `wc -c` | **28673** (expect 28673) |
| Court log | `ALIVE:GATEKEEPER_WASM_SIZE measured=28673 bound=256000` |
| Registry pin | `registry/ARTIFACTS.sha256` — `wasm 5faae464… 28673 affidavit_gatekeeper_verify_witness.wasm` |
| Bound | ≤ 256000 (affidavit gatekeeper court); marketing &lt;250KB / Gate-1 &lt;262144 still hold |
| Probe agent | `bc-5f5786ff-6189-5989-82ba-0a4087074e65` (clause-3 next verifier; tip-wins absorb onto crown) |
| Prior path doc | [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) |
| ggen placeholder court | [`VERIFY-WITNESS-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-SIZE-COURT-STANDING.md) — tip-cargo size fence **ALIVE** @ `f5f44571f` (≠ production) |
| Crown tip (absorb base) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `1dbf1cc2e5a779cbaaaae6ad2075809e5c53be79` (post Smalt/OOR/CASTLE/VW-prod/BRCE-V8 tip-wins; this absorb docs-only) |
| Side | `cursor/v2711-vw-affidavit-court-bc54` @ `cc0ea07e4` (from `9173c6529`) |
| ManagePullRequest | **not** used (no #849) |

## Standing (honest split)

| Claim | Standing |
|-------|----------|
| Affidavit production pin size court (clause 3) | **ALIVE** (EXIT **0**; `wc -c` **28673** @ `563eb322a`) |
| ggen crown production `verify_witness` runtime | **UNSUPPORTED** / **PARTIAL** — receipt absorbed as honesty surface only; `GGEN_VERIFY_WITNESS_WASM` **not** wired on tip |
| Placeholder size court in ggen | unchanged tip-cargo **ALIVE** (size fence only; ≠ production WASM `ALIVE`) |

**Honesty:** affidavit `ALIVE` ≠ automatic ggen production `ALIVE` until `GGEN_VERIFY_WITNESS_WASM` is wired to the measured artifact and a ggen consume court exits **0**. Tip-wins absorb of this ledger does **not** invent ggen production `ALIVE`. Smalt / OOR / CASTLE / VW size-court / BRCE standings on tip are retained.

## Exact next verifier (ggen wire — still open)

```bash
export GGEN_VERIFY_WITNESS_WASM=/abs/path/to/affidavit_gatekeeper_verify_witness.wasm
# then call ggen_graph::witness::verify_production_witness(None)
# OverBudget → WitnessRefusal::OverBudget; missing path → Unsupported
```

Until that path is courted on tip: ggen production remains **UNSUPPORTED**.

## Replay (affidavit pin)

```bash
git clone https://github.com/seanchatmangpt/affidavit.git /tmp/affidavit
cd /tmp/affidavit
git checkout 563eb322a16d805ebc99b2c16f1165462244d7e2
just gatekeeper-wasm-size-court
echo EXIT=$?
wc -c affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
```

Expect: EXIT **0**, bytes **28673**.
