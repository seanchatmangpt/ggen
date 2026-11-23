# Sovereign risk register — R-01 … R-07 (admitted mount)

| Field | Value |
|-------|-------|
| Provenance | Verbatim §4 from `seanchatmangpt/zcode-cli` `docs/ecosystem-spec-27.1.1.md` |
| Source tip | `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a` |
| Mount | `docs/rfc/v27.1.1/fixtures/r01-r07-sovereign-risk-register.md` |
| Law | Do **not** invent prose; this file is the admitted body mount for ECOSYSTEM-SPEC-27.1.1 §11 |

## Explicit Unresolved Engineering Resolution Contracts

| Risk ID | Subsystem | Unresolved Tension | Mandatory Resolution Contract |
| :--- | :--- | :--- | :--- |
| **R-01** | `cos.fnox.io` | 6 ledger conservation flaws + 36-failure `--seed 7` race. | Close all 6 fixes, eliminate `--seed 7` race, pass 2 consecutive LOW-only skeptic waves before blasting 21k LinkedIn graph. |
| **R-02** | `FFB` Web Worker | Popcorn (`beam.wasm` 2.44 MB) + DuckDB-Wasm previously hit 64 MiB vs. 32 MB ceiling. | Lock fixed-size `SharedArrayBuffer` arena partitioning (`initial === maximum`) before mobile OOM under 250k-row loads. |
| **R-03** | `FFB` Transport | Sandboxed browsers cannot dial raw QUIC `ALPN: aoq/1` directly. | Align WebTransport (`h3`) edge-proxy tunneling to native `ALPN: aoq/1` + UDP-443 fallback across code and release docs. |
| **R-04** | `FFB` Auth | Zero-cookie/zero-token reconnect across Wi-Fi ↔ cellular migration. | Bind non-extractable WebCrypto (`extractable: false` in IndexedDB) channel-binding signatures to `:gen_statem` `SessionActor`. |
| **R-05** | `Affidavit` `.aod` | ML-DSA-65 signature (~3,309 B) vs. `CtrtShadow.pqc_seal: [u8; 248]` + FFOR variable bit-width vs. fixed-stride. | Lock detached Epoch Lease anchor encoding and `expected_byte_length` stride contract in `#![no_std]` crate. |
| **R-06** | `zcode-cli` Exploit | Explore heuristics (`m ≡ 0` enforced replay, naked timeout returns). | **CLOSED in `zcode-cli` (`0cff19a`, `7b2c181`)** : optimal A* LP-heuristic alignment (γ = 0), bounded transactional nets + Saga LIFO rollback, and capped remediation retries. |
| **R-07** | Billing Rails | GCP Billing flags prepaid/single-use virtual cards (`018FD7-382078-0D52EF` hold). | Clear GCP billing hold and stand up direct subscription card/ACH rails (Stripe Billing + Mercury/Brex) for multi-cell ramp. |
