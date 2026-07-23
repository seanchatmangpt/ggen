#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
mkdir -p receipts

cargo test --manifest-path production-system/Cargo.toml --all-targets
cargo clippy --manifest-path production-system/Cargo.toml --all-targets -- -D warnings
lake env lean TCPSLifecycle/Production.lean

python3 - <<'PY'
import hashlib
import json
from pathlib import Path

roots = [
    Path("src/production.rs"),
    Path("production-system/Cargo.toml"),
    Path("production-system/src/lib.rs"),
    Path("production-system/tests/production.rs"),
    Path("TCPSLifecycle/Production.lean"),
    Path("scripts/verify-production.sh"),
    Path("deploy/production.toml"),
    Path("release/release.toml"),
    Path("cicd.toml"),
    Path("ggen.toml"),
    Path("schema/domain.ttl"),
    Path("../../packs/tcps-pack/ontology.ttl"),
    Path("../../packs/tcps-pack/pack.toml"),
]
roots.extend(sorted(Path("../../packs/tcps-pack/gates").glob("*.rq")))
roots.extend(sorted(Path("../../packs/tcps-pack/templates").glob("*.tmpl")))

h = hashlib.sha256()
for path in sorted(roots, key=lambda p: str(p)):
    if not path.is_file():
        raise SystemExit(f"required production input missing: {path}")
    data = path.read_bytes()
    h.update(len(str(path)).to_bytes(8, "little"))
    h.update(str(path).encode())
    h.update(len(data).to_bytes(8, "little"))
    h.update(data)

receipt = {
    "schema": "tcps-production-verification/v1",
    "standing": "source-kernel-pass",
    "external_standing": "refused-until-signed-oracles-and-deployment-replay",
    "complete_source_digest": h.hexdigest(),
    "checks": [
        "production-rust-tests",
        "production-clippy-deny-warnings",
        "production-lean-kernel",
        "complete-input-closure",
    ],
}
Path("receipts/production-verification.json").write_text(
    json.dumps(receipt, indent=2) + "\n"
)
print(json.dumps(receipt, indent=2))
PY
