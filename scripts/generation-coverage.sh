#!/usr/bin/env bash
# Generation-coverage metric for the generation-first audit.
# Joins docs/audits/generation-2026-07/classification.tsv (path<TAB>class<TAB>evidence,
# longest-prefix match, directories allowed) against tokei per-file Rust code LOC over
# all workspace members' src/ trees. Fails (exit 1) if any src file is unclassified —
# that failure is the CI ratchet that keeps the manifest complete.
# Output: docs/audits/generation-2026-07/coverage.json (includes git SHA).

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

AUDIT_DIR="docs/audits/generation-2026-07"
TSV="$AUDIT_DIR/classification.tsv"
OUT="$AUDIT_DIR/coverage.json"

[[ -f "$TSV" ]] || { echo "ERROR: $TSV not found" >&2; exit 1; }
command -v tokei >/dev/null || { echo "ERROR: tokei not installed (cargo install tokei)" >&2; exit 1; }

# Workspace member src dirs from cargo metadata (root package's src/ included).
SRC_DIRS=$(cargo metadata --no-deps --format-version=1 2>/dev/null | python3 -c "
import json, sys, os
d = json.load(sys.stdin)
for p in d['packages']:
    src = os.path.join(os.path.dirname(p['manifest_path']), 'src')
    print(os.path.relpath(src))
")

# Per-file Rust code LOC.
tokei $SRC_DIRS --output json --files > /tmp/tokei-files.json

python3 - "$TSV" "$OUT" <<'PY'
import json, sys, subprocess

tsv_path, out_path = sys.argv[1], sys.argv[2]

rules = []  # (path_prefix, class)
with open(tsv_path) as f:
    for line in f:
        line = line.rstrip("\n")
        if not line or line.startswith("#"):
            continue
        parts = line.split("\t")
        if len(parts) < 2:
            continue
        rules.append((parts[0].rstrip("/"), parts[1].strip()))
# Longest prefix wins.
rules.sort(key=lambda r: len(r[0]), reverse=True)

with open("/tmp/tokei-files.json") as f:
    tk = json.load(f)

CLASSES = ["GENERATED", "GENERATABLE-NOW", "GENERATABLE-WITH-SPEC",
           "IRREDUCIBLY-CUSTOM", "DEAD-DELETE"]
totals = {c: 0 for c in CLASSES}
unclassified = []
total_loc = 0

for report in tk.get("Rust", {}).get("reports", []):
    path = report["name"].lstrip("./")
    loc = report["stats"]["code"]
    total_loc += loc
    cls = None
    for prefix, c in rules:
        if path == prefix or path.startswith(prefix + "/"):
            cls = c
            break
    if cls is None:
        unclassified.append(path)
    elif cls in totals:
        totals[cls] += loc
    else:
        print(f"ERROR: unknown class '{cls}' for {path}", file=sys.stderr)
        sys.exit(1)

dead = totals["DEAD-DELETE"]
denom = total_loc - dead
sha = subprocess.run(["git", "rev-parse", "HEAD"], capture_output=True, text=True).stdout.strip()

result = {
    "git_sha": sha,
    "tool": "tokei (Rust code lines, src/ only)",
    "total_loc": total_loc,
    "by_class": totals,
    "generation_coverage": round(totals["GENERATED"] / denom, 4) if denom else 0,
    "potential_coverage": round((totals["GENERATED"] + totals["GENERATABLE-NOW"]
                                 + totals["GENERATABLE-WITH-SPEC"]) / denom, 4) if denom else 0,
    "custom_ratio": round(totals["IRREDUCIBLY-CUSTOM"] / denom, 4) if denom else 0,
    "unclassified_count": len(unclassified),
}
with open(out_path, "w") as f:
    json.dump(result, f, indent=2)
    f.write("\n")

print(json.dumps(result, indent=2))
if unclassified:
    print(f"\nFAIL: {len(unclassified)} unclassified src files (first 20):", file=sys.stderr)
    for p in unclassified[:20]:
        print(f"  {p}", file=sys.stderr)
    sys.exit(1)
print("\nOK: every workspace src file is classified.")
PY
