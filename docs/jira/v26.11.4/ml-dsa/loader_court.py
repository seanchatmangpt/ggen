#!/usr/bin/env python3
"""OI-3 / ZP-22 Phase 1 loader court.

Loads the FIPS-204 parameter scaffold + ML-DSA-65 witness fixtures and
validates them against real cryptography (hashlib.shake_256 = FIPS 202
SHAKE-256 from the Python stdlib — a real collaborator, no mocks).

Gates (fail-closed, exit 1):
  G1  parameters: every parameter set carries all Table-2 fields; ML-DSA-65
      pinned constants {q=8380417, k=6, l=5, eta=4, pk=1952, sig=2420}.
  G2  fixtures: hex fields parse; lengths match the law (xi=32, rho=32,
      sigma=64, K=32); pk_prefix_rho == expected_rho.
  G3  witness law: SHAKE-256(xi || 0x06 || 0x05, 128) reproduces
      expected_rho byte-exact for >= 1 vector (falsifier gate) and for all
      vectors (full corpus).
  G4  anti-vacuity: mutating one fixture rho byte must make the witness
      check fail — the court proves it can fail.
"""
import hashlib
import json
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REQUIRED_PS_FIELDS = [
    "lambda_bits", "n", "q", "d_ropped_bits", "tau", "beta",
    "gamma1", "gamma2", "k", "l", "eta", "pk_bytes", "sig_bytes", "sk_bytes",
]
MLDSA65_PINNED = {"q": 8380417, "k": 6, "l": 5, "eta": 4,
                  "pk_bytes": 1952, "sig_bytes": 2420, "sk_bytes": 4032,
                  "tau": 49, "beta": 196, "gamma1": 131072, "gamma2": 261888,
                  "n": 256}


def fail(msg):
    print(f"COURT-FAIL: {msg}")
    sys.exit(1)


def hx(s, nbytes, ctx):
    if len(s) != 2 * nbytes:
        fail(f"{ctx}: expected {nbytes} bytes, got {len(s)//2}")
    return bytes.fromhex(s)


def expansion_matches(xi, rho):
    """The pinned FIPS-204 final seed-expansion law for ML-DSA-65."""
    d = hashlib.shake_256(xi + bytes([0x06, 0x05])).digest(128)
    return d[:32] == rho


def main():
    ps_doc = json.loads((HERE / "fips204_parameters.json").read_text())
    fx_doc = json.loads((HERE / "witness_fixtures.json").read_text())

    # G1 parameter scaffold
    for name, ps in ps_doc["parameter_sets"].items():
        for f in REQUIRED_PS_FIELDS:
            if f not in ps:
                fail(f"parameter set {name} missing field {f}")
        if ps["q"] != 8380417 or ps["n"] != 256:
            fail(f"{name}: q/n off-spec")
        if ps["gamma1"] != 131072:
            fail(f"{name}: gamma1 off-spec")
    p65 = ps_doc["parameter_sets"]["ML-DSA-65"]
    for f, want in MLDSA65_PINNED.items():
        if p65.get(f) != want:
            fail(f"ML-DSA-65 pinned {f}: want {want}, got {p65.get(f)}")

    # G2 fixture hygiene
    vectors = fx_doc["vectors"]
    if not vectors:
        fail("no witness vectors")
    parsed = []
    for v in vectors:
        xi = hx(v["seed_xi"], 32, v["id"] + " xi")
        rho = hx(v["expected_rho"], 32, v["id"] + " rho")
        sigma = hx(v["expected_sigma"], 64, v["id"] + " sigma")
        k_seed = hx(v["expected_k_seed"], 32, v["id"] + " K")
        if v["pk_prefix_rho"] != v["expected_rho"]:
            fail(f"{v['id']}: pk_prefix_rho != expected_rho")
        parsed.append((v["id"], xi, rho, sigma, k_seed))

    # G3 witness law — the falsifier gate
    for vid, xi, rho, sigma, k_seed in parsed:
        d = hashlib.shake_256(xi + bytes([0x06, 0x05])).digest(128)
        if d[:32] != rho:
            fail(f"{vid}: SHAKE-256(xi||6||5)[:32] != expected_rho")
        if d[32:96] != sigma or d[96:128] != k_seed:
            fail(f"{vid}: sigma/K != expansion")
    if not expansion_matches(parsed[0][1], parsed[0][2]):
        fail("zero vectors validated (falsifier gate)")

    # G4 anti-vacuity: a one-byte fixture mutation must flip the court
    vid, xi, rho, _sigma, _k = parsed[0]
    corrupted = bytearray(rho)
    corrupted[0] ^= 0x01
    if expansion_matches(xi, bytes(corrupted)):
        fail("corrupted rho still validates — witness check is vacuous")

    print(f"COURT-PASS: {len(parsed)}/{len(parsed)} ML-DSA-65 keygen vectors "
          f"witness-validated (SHAKE-256 seed expansion byte-exact vs NIST "
          f"ACVP rho); parameter scaffold OK; anti-vacuity mutation caught")


if __name__ == "__main__":
    main()
