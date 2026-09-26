#!/usr/bin/env bash
set -euo pipefail

# Manufacture connector-transportable ggen cloud capsules.
#
# The CI job owns the build toolchain. This manufacturer transports the exact
# executable subject plus source closure, without copying transient compiler
# caches into the runtime artifact. Each emitted artifact is hard-bounded below
# the connector's 512 MiB admission ceiling.

repo_root="$(git rev-parse --show-toplevel)"
lock_file="${GGEN_ECOSYSTEM_LOCK:-$repo_root/cloud/ggen-ecosystem.lock.toml}"
out_dir="${1:-${RUNNER_TEMP:-/tmp}/ggen-cloud-environment-out}"
dfcm_binary="${DFCM_TEST_BINARY:-}"
ggen_binary="${GGEN_BINARY:-$repo_root/target/debug/ggen}"
max_transport_bytes=$((500 * 1024 * 1024))

refuse() {
  printf 'REFUSED[%s]: %s\n' "$1" "$2" >&2
  exit 2
}

for tool in git python3 tar gzip rustc cargo sha256sum; do
  command -v "$tool" >/dev/null || refuse MISSING_TOOL "$tool is required"
done
[[ -f "$lock_file" ]] || refuse MISSING_LOCK "$lock_file does not exist"
[[ -x "$ggen_binary" ]] || refuse MISSING_GGEN_BINARY "$ggen_binary is not executable"
[[ -n "$dfcm_binary" && -x "$dfcm_binary" ]] || refuse MISSING_DFCM_BINARY \
  "DFCM_TEST_BINARY must name the compiled dfcm_crown_suite executable"

candidate_sha="$(git -C "$repo_root" rev-parse HEAD)"
[[ "$candidate_sha" =~ ^[0-9a-f]{40}$ ]] || refuse INVALID_CANDIDATE_SHA "$candidate_sha"

readarray -t lock_rows < <(python3 - "$lock_file" <<'PY'
import sys, tomllib
from pathlib import Path
p = Path(sys.argv[1])
data = tomllib.loads(p.read_text())
if data.get("schema") != "ggen.ecosystem-lock.v1":
    raise SystemExit("REFUSED[LOCK_SCHEMA]: expected ggen.ecosystem-lock.v1")
owner = data.get("owner")
if not isinstance(owner, str) or not owner:
    raise SystemExit("REFUSED[LOCK_OWNER]: owner is required")
print(f"OWNER\t{owner}")
seen = set()
for repo in data.get("repository", []):
    name = repo.get("name")
    sha = repo.get("sha")
    role = repo.get("role", "source")
    if not isinstance(name, str) or not name or name in seen:
        raise SystemExit(f"REFUSED[LOCK_REPOSITORY]: invalid/duplicate name {name!r}")
    if not isinstance(sha, str) or len(sha) != 40 or any(c not in "0123456789abcdef" for c in sha):
        raise SystemExit(f"REFUSED[LOCK_SHA]: {name} has invalid sha {sha!r}")
    seen.add(name)
    print(f"REPO\t{name}\t{sha}\t{role}")
PY
)
[[ "${#lock_rows[@]}" -gt 1 ]] || refuse EMPTY_LOCK "no ecosystem repositories admitted"
owner="${lock_rows[0]#OWNER$'\t'}"

rm -rf "$out_dir"
mkdir -p "$out_dir"
stage="$out_dir/.stage"
mkdir -p "$stage/bin" "$stage/workspace" "$stage/receipts"
identities="$stage/receipts/source-identities.tsv"
printf 'repository\tsha\trole\ttransport\n' > "$identities"

archive_tree() {
  local source_repo="$1" ref="$2" destination="$3"
  mkdir -p "$destination"
  git -C "$source_repo" archive "$ref" | tar -xf - -C "$destination"
}

archive_tree "$repo_root" "$candidate_sha" "$stage/workspace/ggen"
printf 'ggen\t%s\tcandidate-source\tlocal-exact-tree\n' "$candidate_sha" >> "$identities"

runtime_siblings=(lsp-max lsp-types-max wasm4pm wasm4pm-compat)
repo_parent="$(dirname "$repo_root")"
for name in "${runtime_siblings[@]}"; do
  sibling="$repo_parent/$name"
  [[ -d "$sibling/.git" ]] || refuse MISSING_RUNTIME_SIBLING "$name not provisioned next to ggen"
  sha="$(git -C "$sibling" rev-parse HEAD)"
  [[ "$sha" =~ ^[0-9a-f]{40}$ ]] || refuse INVALID_RUNTIME_SIBLING_SHA "$name=$sha"
  archive_tree "$sibling" "$sha" "$stage/workspace/$name"
  printf '%s\t%s\truntime-path-dependency\tsetup-ggen-build\n' "$name" "$sha" >> "$identities"
done

source_repos=()
for row in "${lock_rows[@]:1}"; do
  IFS=$'\t' read -r kind name sha role <<< "$row"
  [[ "$kind" == "REPO" ]] || refuse INVALID_LOCK_ROW "$row"
  tmp="$(mktemp -d)"
  git init -q "$tmp"
  git -C "$tmp" remote add origin "https://github.com/$owner/$name"
  git -C "$tmp" fetch -q --depth 1 origin "$sha"
  actual="$(git -C "$tmp" rev-parse FETCH_HEAD)"
  [[ "$actual" == "$sha" ]] || refuse SOURCE_IDENTITY_MISMATCH "$name expected=$sha actual=$actual"
  archive_tree "$tmp" "$actual" "$stage/workspace/$name"
  printf '%s\t%s\t%s\texact-sha-fetch\n' "$name" "$actual" "$role" >> "$identities"
  source_repos+=("$name")
  rm -rf "$tmp"
done

cp "$ggen_binary" "$stage/bin/ggen"
cp "$dfcm_binary" "$stage/bin/dfcm-crown-suite"
chmod +x "$stage/bin/ggen" "$stage/bin/dfcm-crown-suite"

# Execute the exact candidate in CI before manufacturing transport projections.
ggen_version="$($stage/bin/ggen --version 2>&1)"
rustc_version="$(rustc --version 2>&1)"
cargo_version="$(cargo --version 2>&1)"
dfcm_output="$($stage/bin/dfcm-crown-suite --exact dfcm_crown_suite_completes_under_5_seconds --nocapture 2>&1)"
(
  cd "$stage/workspace/ggen"
  "$stage/bin/ggen" graph validate >/dev/null
  cargo metadata --offline --no-deps --manifest-path Cargo.toml >/dev/null
)

GGEN_VERSION="$ggen_version" RUSTC_VERSION="$rustc_version" \
CARGO_VERSION="$cargo_version" DFCM_OUTPUT="$dfcm_output" \
CANDIDATE_SHA="$candidate_sha" python3 - "$identities" "$stage/receipts/environment.json" <<'PY'
import json, os, platform, sys
from pathlib import Path
identities_path, out = map(Path, sys.argv[1:3])
rows = []
for line in identities_path.read_text().splitlines()[1:]:
    repository, sha, role, transport = line.split("\t")
    rows.append({"repository": repository, "sha": sha, "role": role, "transport": transport})
receipt = {
    "schema": "ggen.cloud-environment-receipt.v2",
    "standing": "ALIVE",
    "scope": "executed ggen + DfCM runtime with exact-source ecosystem closure",
    "source_closure_standing": "PARTIAL_ALIVE",
    "candidate_sha": os.environ["CANDIDATE_SHA"],
    "platform": {"machine": platform.machine(), "system": platform.system()},
    "observed": {
        "sources": rows,
        "ggen_version": os.environ["GGEN_VERSION"],
        "rustc_version": os.environ["RUSTC_VERSION"],
        "cargo_version": os.environ["CARGO_VERSION"],
    },
    "executed": [
        "bin/ggen --version",
        "bin/dfcm-crown-suite --exact dfcm_crown_suite_completes_under_5_seconds --nocapture",
        "bin/ggen graph validate (workspace/ggen)",
        "cargo metadata --offline --no-deps --manifest-path workspace/ggen/Cargo.toml",
    ],
    "dfcm": {
        "standing": "ALIVE",
        "test": "dfcm_crown_suite_completes_under_5_seconds",
        "output": os.environ["DFCM_OUTPUT"],
    },
    "transport": {
        "toolchain_transported": False,
        "cargo_cache_transported": False,
        "reason": "compiler/toolchain authority remains in setup-ggen-build; cloud runtime transports executable subject and exact sources",
    },
    "authority": {
        "select": "exact candidate SHA + admitted lock + setup-ggen-build sibling heads",
        "construct": "bounded runtime capsule + independently receipted source capsules",
        "do": "bounded verifier only",
        "external_actuation": False,
    },
}
out.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n")
PY

runtime="$out_dir/ggen-cloud-runtime"
mkdir -p "$runtime/bin" "$runtime/workspace" "$runtime/receipts"
cp -a "$stage/bin/." "$runtime/bin/"
cp -a "$stage/receipts/." "$runtime/receipts/"
for name in ggen "${runtime_siblings[@]}"; do
  cp -a "$stage/workspace/$name" "$runtime/workspace/$name"
done

cat > "$runtime/activate.sh" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export GGEN_CLOUD_ENV_ROOT="$root"
export GGEN_ECOSYSTEM_ROOT="$root/workspace"
export PATH="$root/bin:$PATH"
SH
chmod +x "$runtime/activate.sh"

cat > "$runtime/verify.sh" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$root/activate.sh"
(
  cd "$root"
  sha256sum -c MANIFEST.sha256
)
"$root/bin/ggen" --version
"$root/bin/dfcm-crown-suite" --exact dfcm_crown_suite_completes_under_5_seconds --nocapture
(
  cd "$root/workspace/ggen"
  "$root/bin/ggen" graph validate
)
for repository in ggen lsp-max lsp-types-max wasm4pm wasm4pm-compat; do
  [[ -d "$root/workspace/$repository" ]] || {
    echo "REFUSED[MISSING_RUNTIME_SOURCE]: $repository" >&2
    exit 2
  }
done
printf 'ALIVE ggen-cloud-runtime\n'
SH
chmod +x "$runtime/verify.sh"

cat > "$runtime/verify-closure.sh" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
"$root/verify.sh"
while IFS=$'\t' read -r repository sha role transport; do
  [[ "$repository" == "repository" ]] && continue
  [[ -d "$root/workspace/$repository" ]] || {
    echo "REFUSED[MISSING_SOURCE_TREE]: $repository" >&2
    exit 2
  }
done < "$root/receipts/source-identities.tsv"
printf 'ALIVE ggen-cloud-environment source_closure=PARTIAL_ALIVE\n'
SH
chmod +x "$runtime/verify-closure.sh"

(
  cd "$runtime"
  find . -type f ! -name MANIFEST.sha256 -print0 | sort -z | xargs -0 sha256sum > MANIFEST.sha256
)
"$runtime/verify.sh"

pack_dir() {
  local parent="$1" name="$2" archive="$3"
  (
    cd "$parent"
    tar --sort=name --mtime='UTC 2026-08-19' --owner=0 --group=0 --numeric-owner \
      -cf - "$name" | gzip -n -3 > "$archive"
  )
}

runtime_archive="$out_dir/ggen-cloud-runtime.tar.gz"
pack_dir "$out_dir" ggen-cloud-runtime "$runtime_archive"
(
  cd "$out_dir"
  sha256sum "$(basename "$runtime_archive")" > "$(basename "$runtime_archive").sha256"
)

for name in "${source_repos[@]}"; do
  archive="$out_dir/ggen-source-$name.tar.gz"
  pack_dir "$stage/workspace" "$name" "$archive"
  (
    cd "$out_dir"
    sha256sum "$(basename "$archive")" > "$(basename "$archive").sha256"
  )
done

python3 - "$out_dir" "$max_transport_bytes" <<'PY'
import hashlib, json, sys
from pathlib import Path
root = Path(sys.argv[1])
limit = int(sys.argv[2])
entries = []
for path in sorted(root.glob("ggen-*.tar.gz")):
    size = path.stat().st_size
    if size > limit:
        raise SystemExit(f"REFUSED[TRANSPORT_TOO_LARGE]: {path.name}={size} limit={limit}")
    digest_state = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            digest_state.update(chunk)
    digest = digest_state.hexdigest()
    entries.append({"file": path.name, "bytes": size, "sha256": digest})
(root / "transport-index.json").write_text(json.dumps({
    "schema": "ggen.cloud-transport-index.v1",
    "max_transport_bytes": limit,
    "artifacts": entries,
}, indent=2, sort_keys=True) + "\n")
PY
cp "$stage/receipts/environment.json" "$out_dir/environment.json"

# Replay the exact transport projections, then assemble the broader source
# closure and re-run the bounded closure verifier.
replay="$out_dir/.replay"
mkdir -p "$replay"
(
  cd "$out_dir"
  sha256sum -c ggen-cloud-runtime.tar.gz.sha256
)
tar -xzf "$runtime_archive" -C "$replay"
"$replay/ggen-cloud-runtime/verify.sh"
for name in "${source_repos[@]}"; do
  (
    cd "$out_dir"
    sha256sum -c "ggen-source-$name.tar.gz.sha256"
  )
  tar -xzf "$out_dir/ggen-source-$name.tar.gz" -C "$replay/ggen-cloud-runtime/workspace"
done
"$replay/ggen-cloud-runtime/verify-closure.sh"
printf '%s\n' "$out_dir/transport-index.json"
