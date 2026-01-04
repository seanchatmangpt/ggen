#!/usr/bin/env bash
set -euo pipefail

# Rebuild marketplace/registry/index.json from package manifests.
# Usage: scripts/diagnostics/registry-rebuild.sh [registry_dir] [packages_dir]
# Defaults:
#   registry_dir: marketplace/registry
#   packages_dir: marketplace/packages
#
# Requirements: python 3.11+ (uses tomllib)

registry_dir="${1:-marketplace/registry}"
packages_dir="${2:-marketplace/packages}"
index_path="${registry_dir}/index.json"

if [[ ! -d "${packages_dir}" ]]; then
  echo "Packages directory not found: ${packages_dir}" >&2
  exit 1
fi

mkdir -p "${registry_dir}"

python3 - <<'PYCODE' "${registry_dir}" "${packages_dir}"
import json
import sys
import tomllib
from pathlib import Path

if len(sys.argv) < 3:
    print("usage: registry-rebuild.py <registry_dir> <packages_dir>", file=sys.stderr)
    sys.exit(1)

registry_dir = Path(sys.argv[1])
packages_dir = Path(sys.argv[2])
index_path = registry_dir / "index.json"

packages = []

for pkg_toml in sorted(packages_dir.glob("*/package.toml")):
    try:
        data = tomllib.loads(pkg_toml.read_text())
    except Exception as exc:  # noqa: BLE001
        print(f"skip {pkg_toml}: {exc}", file=sys.stderr)
        continue

    name = data.get("package", {}).get("name") or data.get("name")
    if not name:
        print(f"skip {pkg_toml}: missing package.name", file=sys.stderr)
        continue

    versions = data.get("package", {}).get("versions")
    if versions is None:
        # Try common patterns
        versions = data.get("versions") or data.get("releases") or []

    # Ensure list of strings
    clean_versions = []
    for v in versions or []:
        if isinstance(v, str):
            clean_versions.append(v)
        elif isinstance(v, dict) and "version" in v:
            clean_versions.append(str(v["version"]))
    clean_versions = sorted(set(clean_versions))

    packages.append(
        {
            "name": name,
            "path": str(pkg_toml.parent),
            "versions": clean_versions,
        }
    )

index = {
    "packages": packages,
    "package_count": len(packages),
    "version_count": sum(len(p["versions"]) for p in packages),
}

index_path.write_text(json.dumps(index, indent=2, sort_keys=True))

print(f"wrote {index_path}")
print(f"package_count={index['package_count']}")
print(f"version_count={index['version_count']}")
PYCODE

# Post-summary using existing summary script if available
if [[ -x "scripts/diagnostics/registry-summary.sh" ]]; then
  bash scripts/diagnostics/registry-summary.sh "${index_path}"
fi






