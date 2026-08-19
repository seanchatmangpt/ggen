#!/usr/bin/env bash
set -euo pipefail

# Manufacture a portable, exact-source ggen execution capsule.
#
# Authority boundaries:
# - the checked-out ggen commit is the candidate identity;
# - cloud/ggen-ecosystem.lock.toml admits the broader source closure;
# - setup-ggen-build owns the four path-dependency sibling SHAs;
# - this script CONSTRUCTS a capsule and executes only its bounded verifier.
# It does not publish, deploy, mutate external systems, or infer missing SHAs.

repo_root="$(git rev-parse --show-toplevel)"
lock_file="${GGEN_ECOSYSTEM_LOCK:-$repo_root/cloud/ggen-ecosystem.lock.toml}"
out_dir="${1:-${RUNNER_TEMP:-/tmp}/ggen-cloud-environment-out}"
dfcm_binary="${DFCM_TEST_BINARY:-}"
owner=""

refuse() {
  printf 'REFUSED[%s]: %s\n' "$1" "$2" >&2
  exit 2
}

command -v git >/dev/null || refuse MISSING_GIT "git is required"
command -v python3 >/dev/null || refuse MISSING_PYTHON "python3 is required"
command -v tar >/dev/null || refuse MISSING_TAR "tar is required"
command -v gzip >/dev/null || refuse MISSING_GZIP "gzip is required"
command -v rustc >/dev/null || refuse MISSING_RUSTC "rustc is required"
command -v cargo >/dev/null || refuse MISSING_CARGO "cargo is required"
[[ -f "$lock_file" ]] || refuse MISSING_LOCK "$lock_file does not exist"
[[ -n "$dfcm_binary" && -x "$dfcm_binary" ]] || refuse MISSING_DFCM_BINARY \
  "DFCM_TEST_BINARY must name the compiled dfcm_crown_suite executable"
[[ -x "$repo_root/target/release/ggen" ]] || refuse MISSING_GGEN_BINARY \
  "target/release/ggen must be built before manufacture"

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
stage="$out_dir/ggen-cloud-environment"
mkdir -p "$stage/bin" "$stage/toolchain" "$stage/cargo-home" \
  "$stage/workspace" "$stage/receipts"
identities="$stage/receipts/source-identities.tsv"
printf 'repository\tsha\trole\ttransport\n' > "$identities"

archive_tree() {
  local source_repo="$1" ref="$2" destination="$3"
  mkdir -p "$destination"
  git -C "$source_repo" archive "$ref" | tar -xf - -C "$destination"
}

# Exact candidate source.
archive_tree "$repo_root" "$candidate_sha" "$stage/workspace/ggen"
printf 'ggen\t%s\tcandidate-source\tlocal-exact-tree\n' "$candidate_sha" >> "$identities"

# The setup action is authoritative for these path dependencies. Record what
# was actually materialized; do not duplicate its pins in a second config.
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

# Broader ecosystem source closure: fetch ONLY admitted exact SHAs.
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
  rm -rf "$tmp"
done

# Runtime capsule. Copy the rustup sysroot instead of relying on rustup being
# present in the consumer. Copy only Cargo caches, never credentials/config tokens.
sysroot="$(rustc --print sysroot)"
[[ -d "$sysroot/bin" ]] || refuse INVALID_RUST_SYSROOT "$sysroot"
cp -a "$sysroot"/. "$stage/toolchain/"
cp "$repo_root/target/release/ggen" "$stage/bin/ggen"
cp "$dfcm_binary" "$stage/bin/dfcm-crown-suite"
chmod +x "$stage/bin/ggen" "$stage/bin/dfcm-crown-suite"

for tool in sccache just; do
  if path="$(command -v "$tool" 2>/dev/null)"; then
    cp "$path" "$stage/bin/$tool"
    chmod +x "$stage/bin/$tool"
  fi
done

cargo_home="${CARGO_HOME:-$HOME/.cargo}"
for rel in registry/cache registry/index registry/src git/db git/checkouts; do
  if [[ -e "$cargo_home/$rel" ]]; then
    mkdir -p "$stage/cargo-home/$(dirname "$rel")"
    cp -a "$cargo_home/$rel" "$stage/cargo-home/$rel"
  fi
done

cat > "$stage/activate.sh" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export GGEN_CLOUD_ENV_ROOT="$root"
export GGEN_ECOSYSTEM_ROOT="$root/workspace"
export CARGO_HOME="$root/cargo-home"
export CARGO_NET_OFFLINE=true
export PATH="$root/toolchain/bin:$root/bin:$PATH"
export LD_LIBRARY_PATH="$root/toolchain/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
SH
chmod +x "$stage/activate.sh"

cat > "$stage/verify.sh" <<'SH'
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
"$root/bin/dfcm-crown-suite" \
  --exact dfcm_crown_suite_completes_under_5_seconds --nocapture
(
  cd "$root/workspace/ggen"
  "$root/bin/ggen" graph validate
)
cargo metadata --offline --no-deps \
  --manifest-path "$root/workspace/ggen/Cargo.toml" >/dev/null
while IFS=$'\t' read -r repository sha role transport; do
  [[ "$repository" == "repository" ]] && continue
  [[ -d "$root/workspace/$repository" ]] || {
    echo "REFUSED[MISSING_SOURCE_TREE]: $repository" >&2
    exit 2
  }
done < "$root/receipts/source-identities.tsv"
printf 'ALIVE ggen-cloud-environment\n'
SH
chmod +x "$stage/verify.sh"

# Execute the exact staged subject before giving it standing.
ggen_version="$($stage/bin/ggen --version 2>&1)"
rustc_version="$($stage/toolchain/bin/rustc --version 2>&1)"
cargo_version="$($stage/toolchain/bin/cargo --version 2>&1)"
dfcm_output="$($stage/bin/dfcm-crown-suite --exact dfcm_crown_suite_completes_under_5_seconds --nocapture 2>&1)"
(
  # shellcheck disable=SC1091
  source "$stage/activate.sh"
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
    "schema": "ggen.cloud-environment-receipt.v1",
    "standing": "ALIVE",
    "scope": "ggen runtime + DfCM verifier + exact-source ecosystem closure",
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
    "authority": {
        "select": "exact candidate SHA + admitted lock + setup-ggen-build sibling heads",
        "construct": "portable source/toolchain/runtime capsule",
        "do": "bounded local verifier only",
        "external_actuation": False,
    },
}
out.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n")
PY

# Manifest binds every constructed byte except the manifest itself.
(
  cd "$stage"
  find . -type f ! -name MANIFEST.sha256 -print0 | sort -z | xargs -0 sha256sum > MANIFEST.sha256
)

# Replay from the staged capsule before transport packaging.
"$stage/verify.sh"

archive="$out_dir/ggen-cloud-environment.tar.gz"
(
  cd "$out_dir"
  tar --sort=name --mtime='UTC 2026-08-19' --owner=0 --group=0 --numeric-owner \
    -cf - ggen-cloud-environment | gzip -n -9 > "$archive"
)
sha256sum "$archive" > "$archive.sha256"
cp "$stage/receipts/environment.json" "$out_dir/environment.json"
printf '%s\n' "$archive"
