#!/bin/sh
set -eu
root="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
case "${1:-paths}" in
  paths)
    printf 'GGEN_BIN=%s\n' "$root/bin/ggen"
    printf 'GGEN_PACKAGE_MANIFEST=%s\n' "$root/ggen-package-manifest.json"
    ;;
  exec)
    shift
    exec "$root/bin/ggen" "$@"
    ;;
  *)
    echo "usage: bootstrap.sh [paths|exec [ggen args...]]" >&2
    exit 2
    ;;
esac
