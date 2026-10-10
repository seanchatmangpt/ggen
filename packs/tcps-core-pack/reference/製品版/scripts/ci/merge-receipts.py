#!/usr/bin/env python3
from __future__ import annotations
import json, sys
from pathlib import Path
output = Path(sys.argv[1])
inputs = [Path(value) for value in sys.argv[2:]]
records = []
for root in inputs:
    for path in sorted(root.rglob("*.json")):
        try:
            records.append(json.loads(path.read_text(encoding="utf-8")))
        except (json.JSONDecodeError, UnicodeDecodeError):
            pass
output.parent.mkdir(parents=True, exist_ok=True)
output.write_text(json.dumps({"receipts": records}, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
