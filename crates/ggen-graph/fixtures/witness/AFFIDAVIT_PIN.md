# Affidavit gatekeeper pin fixture

| Field | Value |
|-------|-------|
| Artifact | `affidavit_gatekeeper_verify_witness.pin.wasm` |
| Bytes | **28673** |
| SHA-256 | `5faae4642ca23c503e96d2a2ab938c056caffdde14e77516054fe4492aed0c97` |
| Affidavit tip | `563eb322a16d805ebc99b2c16f1165462244d7e2` |
| Role | CI-mount CONSTRUCT consume for OOR **G2-11** |

Owning plane: **affidavit** (`affidavit-gatekeeper-wasm`). ggen mounts these
bytes for `GGEN_VERIFY_WITNESS_WASM` consume courts only — this is **not**
Plane-4 DO and does **not** invent production runtime `ALIVE` without CI EXIT 0.

Refuse the fat `affidavit-wasm` (~718736 B) as the &lt;250KB gatekeeper product.
