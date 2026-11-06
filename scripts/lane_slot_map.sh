#!/bin/sh
# lane_slot_map.sh — project docs/jira/<milestone>/lane-registry.json into a
# lane-slot table (lane | status | lease | owned paths). Read-only projection;
# the registry stays the single source of truth (coordinator-minted).
#
# Usage: scripts/lane_slot_map.sh [registry.json]
#   (default: newest docs/jira/*/lane-registry.json, else .ggen/lane-registry.json)
command -v python3 >/dev/null 2>&1 || { echo "python3 required" >&2; exit 2; }

REG="$1"
if [ -z "$REG" ]; then
  REG="$(ls -t docs/jira/*/lane-registry.json 2>/dev/null | head -1)"
fi
[ -n "$REG" ] && [ -f "$REG" ] || REG=".ggen/lane-registry.json"
[ -f "$REG" ] || { echo "lane_slot_map: no lane-registry.json found" >&2; exit 1; }

python3 - "$REG" <<'EOF'
import json, sys

reg = json.load(open(sys.argv[1]))
print(f"milestone: {reg.get('milestone')}  repo: {reg.get('repo')}")
print(f"{'LANE':<14} {'STATUS':<10} {'LEASE EXPIRES':<20} OWNED PATHS")
for lane in sorted(reg.get("lanes", []), key=lambda l: str(l.get("lane_id"))):
    exp = lane.get("lease_expires") or "-"
    paths = ", ".join(lane.get("owned_paths", [])) or "-"
    print(f"{lane.get('lane_id',''):<14} {lane.get('status',''):<10} {exp:<20} {paths}")
EOF
