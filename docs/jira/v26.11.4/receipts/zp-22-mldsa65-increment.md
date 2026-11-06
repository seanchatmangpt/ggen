# ZP-22 ML-DSA-65 signer — first increment (OI-3, lane L11, wave-3)

Standing: PARTIAL_ALIVE

Date: 2026-11-05. Repo: /Users/sac/ggen (working tree only, NO commits — coordinator owns git).

## Scope landed (this increment)

- New workspace crate `crates/ggen-dsa/` (publish=false): ML-DSA-65 (FIPS-204) signer
  wrapping the RustCrypto `ml-dsa` crate (0.1.2, pure Rust) — prior-art-first; the
  lattice arithmetic is NOT hand-rolled.
- Surfaces: `signing_key_from_seed` / `sign` (deterministic, with context) / `verify`
  (fail-closed on malformed encodings) / `verifying_key_bytes` / `verifying_key_from_seed`;
  FIPS-204 size consts (sig 3309 B, vk 1952 B, seed 32 B).
- Known-answer test against a real referent: the ml-dsa-0.1.2 published example key pair
  (seed 0x00..0x1f; pk extracted from the published SPKI PEM → DER → raw 1952 B hex,
  fixture at `crates/ggen-dsa/tests/fixtures_ml_dsa_65_pk.hex`).
- Root Cargo.toml members += "crates/ggen-dsa"; Cargo.lock additive-only (ggen-dsa +
  ml-dsa dep tree: der/keccak/ml-dsa/module-lattice/pkcs8/shake/spki/sponge-cursor).

## Commands / exits (real)

- `cargo test -p ggen-dsa` → **exit 0**; 2 unit + 6 KAT/falsifier tests, 0 failed
  (fresh run after warning fix; 8/8 green).
- `git diff Cargo.lock` census: additive-only (+103 lines, no upstream churn).

## Falsifiers (all flipping, witnessed by passing negative tests)

- Tampered message → refused; tampered context → refused; wrong verifying key → refused;
  wrong seed → divergent pk; truncated+re-padded signature encoding → refused
  (malformed-decode is fail-closed, not silently accepted).
- Register falsifier "sign-loop court flipping on a faulted candidate count" NOT
  discharged — the FS-abort sign loop is a hand-rolled-phase artifact and does not exist
  in this increment (see Remaining).

## Premise finding (premise rot, reported to register)

PLANS.md:135 ZP-22 premised "reusing the landed field kernel (fips203/fips202)". No such
kernel exists in-tree (repo-wide grep for fips203/fips202/fips_203/fips_202: zero hits,
2026-11-05). Prior-art-first therefore routed to the published RustCrypto crate; the
wave-3 de-risk claim "third-party crate availability ... makes the in-repo answer
vacuous" is inverted: the third-party crate IS the in-repo answer now.

## Remaining (explicit, not hidden)

- ZP-22 phases 2–5 (branchless SampleInBall/SampleMask, FS-abort sign loop with constant
  candidate count + masked index fold, 2420-B serializer onto the frozen Receipt shape,
  const-assert fence) — deferred; the register's sign-loop court targets those phases.
- Receipt-chain integration: no ggen receipt surface consumes ggen-dsa yet (sign-loop
  court onto the frozen Receipt shape is the remaining acceptance).
- Full NIST ACVP KAT corpus (only the single published example-key vector is embedded;
  Wycheproof/NIST JSON corpus not vendored).
- Register row OI-3 / backlog PLANNED status untouched (coordinator owns transitions).

## Lane ownership / disclosures

- Files touched: root Cargo.toml (one members entry + comment), Cargo.lock (regenerated,
  additive), crates/ggen-dsa/** (new, lane-owned), receipt file itself.
- Shared-file note: Cargo.toml/Cargo.lock were already modified in the working tree by
  other lanes before this session (F2 tera dev-dep, register OI-2 discloses this); my
  hunks are additive and separate.
