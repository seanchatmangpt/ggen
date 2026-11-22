# `verify_witness` production WASM — affidavit size court (clause 3)

```text
exit=0 standing=ALIVE(affidavit_production_pin) bytes=28673 pin=563eb322a
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
| Probe agent | `bc-5f5786ff-6189-5989-82ba-0a4087074e65` (clause-3 next verifier; no crown race) |
| Prior path doc | [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) |
| ggen placeholder court | [`VERIFY-WITNESS-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-SIZE-COURT-STANDING.md) — size fence only |
| ManagePullRequest | **not** used (no #849) |

## Standing (honest split)

| Claim | Standing |
|-------|----------|
| Affidavit production pin size court (clause 3) | **ALIVE** |
| ggen crown production `verify_witness` runtime | **not ALIVE** — crown has not absorbed this receipt; `GGEN_VERIFY_WITNESS_WASM` not wired on tip |
| Placeholder size court in ggen | unchanged `PARTIAL_ALIVE` / tip-cargo size fence only |

**Honesty:** affidavit `ALIVE` ≠ automatic ggen production `ALIVE` until crown absorbs and `GGEN_VERIFY_WITNESS_WASM` is wired to the measured artifact.

## Replay

```bash
git clone https://github.com/seanchatmangpt/affidavit.git /tmp/affidavit
cd /tmp/affidavit
git checkout 563eb322a16d805ebc99b2c16f1165462244d7e2
just gatekeeper-wasm-size-court
echo EXIT=$?
wc -c affidavit-gatekeeper-wasm/dist/affidavit_gatekeeper_verify_witness.wasm
```

Expect: EXIT **0**, bytes **28673**.
