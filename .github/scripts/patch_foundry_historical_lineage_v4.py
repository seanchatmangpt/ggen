#!/usr/bin/env python3
"""Execute the v3 patch controller with its dedented insertion anchor."""
from pathlib import Path

path = Path(".github/scripts/patch_foundry_historical_lineage_v3.py")
source = path.read_text(encoding="utf-8")
source = source.replace(
    'marker = "          required = [\\n"',
    'marker = "required = [\\n"',
    1,
)
exec(compile(source, str(path), "exec"), {"__name__": "__main__"})
