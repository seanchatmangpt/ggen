#!/usr/bin/env bash
set -euo pipefail

# Quick diagnostics for marketplace registry index.json
# Usage: scripts/diagnostics/registry-summary.sh [path-to-index.json]

INDEX_PATH="${1:-marketplace/registry/index.json}"

if [[ ! -f "${INDEX_PATH}" ]]; then
  echo "Registry index not found at ${INDEX_PATH}" >&2
  exit 1
fi

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required but not installed" >&2
  exit 1
fi

FILE_SIZE_BYTES=$(wc -c < "${INDEX_PATH}")
FILE_SIZE_KB=$((FILE_SIZE_BYTES / 1024))

# Be tolerant of different shapes; prefer .packages if present, else root array
PACKAGE_COUNT=$(jq -r 'if type=="array" then length else (.packages // []) | length end' "${INDEX_PATH}")
VERSION_COUNT=$(jq -r '
  def pkg_versions:
    if type=="array" then .
    else .packages // []
    end;
  pkg_versions
  | map(.versions // []) | add | length
' "${INDEX_PATH}")

echo "registry_path=${INDEX_PATH}"
echo "file_size_kb=${FILE_SIZE_KB}"
echo "package_count=${PACKAGE_COUNT}"
echo "version_count=${VERSION_COUNT}"

# Top 5 packages by version count (best effort)
jq -r '
  def pkg_versions:
    if type=="array" then .
    else .packages // []
    end;
  pkg_versions
  | map({name: (.name // .id // "unknown"), versions: ((.versions // []) | length)})
  | sort_by(.versions) | reverse
  | .[:5]
  | .[] | "\(.name)=\(.versions)"
' "${INDEX_PATH}" || true
