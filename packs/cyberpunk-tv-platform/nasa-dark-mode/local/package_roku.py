#!/usr/bin/env python3
from __future__ import annotations

import hashlib
import json
import zipfile
from pathlib import Path

PACK = Path(__file__).resolve().parent.parent
ROOT = PACK / "generated/roku"
OUTPUT = PACK / "generated/.ggen/evidence/nasa-dark-mode-roku.zip"
EVIDENCE = PACK / ".ggen/evidence/no-ci"

files = sorted(
    path for path in ROOT.rglob("*")
    if path.is_file() and path.relative_to(ROOT).parts[0] in {"manifest", "source", "components", "data"}
)
OUTPUT.parent.mkdir(parents=True, exist_ok=True)
with zipfile.ZipFile(OUTPUT, "w", compression=zipfile.ZIP_DEFLATED, compresslevel=9) as archive:
    for path in files:
        relative = path.relative_to(ROOT).as_posix()
        info = zipfile.ZipInfo(relative, date_time=(1980, 1, 1, 0, 0, 0))
        info.compress_type = zipfile.ZIP_DEFLATED
        info.create_system = 3
        info.external_attr = 0o100644 << 16
        archive.writestr(info, path.read_bytes(), compress_type=zipfile.ZIP_DEFLATED, compresslevel=9)

digest = hashlib.sha256(OUTPUT.read_bytes()).hexdigest()
report = {
    "schema": "ggen.nasa-dark-mode.deterministic-roku-package.v1",
    "standing": "ALIVE",
    "path": str(OUTPUT),
    "entries": [path.relative_to(ROOT).as_posix() for path in files],
    "bytes": OUTPUT.stat().st_size,
    "sha256": digest,
    "timestampPolicy": "DOS_EPOCH_1980-01-01T00:00:00",
    "ordering": "LEXICOGRAPHIC",
}
EVIDENCE.mkdir(parents=True, exist_ok=True)
(EVIDENCE / "roku-package.json").write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
print(json.dumps(report, sort_keys=True))
