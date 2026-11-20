#!/usr/bin/env bash
# normalize-macho.sh <binary>
# Zero the Mach-O LC_UUID payload, then drop and re-apply the ad-hoc signature so the
# signature hashes cover the normalized bytes. No-op on non-Mach-O files (ELF etc).
# DETB-4: linker-derived LC_UUID depends on the output file name (cargo metadata hash
# includes the checkout root), which breaks cross-path byte identity on macOS.
set -euo pipefail
BIN="${1:?usage: normalize-macho.sh <binary>}"
[ -f "$BIN" ] || { echo "normalize-macho: no such file: $BIN" >&2; exit 2; }

RESULT=$(python3 - "$BIN" <<'PY'
import struct, sys
path = sys.argv[1]
with open(path, "r+b") as f:
    data = bytearray(f.read())
    magic = struct.unpack_from("<I", data, 0)[0]
    if magic not in (0xFEEDFACF,):  # 64-bit little-endian Mach-O only
        print("skip"); sys.exit(0)
    ncmds = struct.unpack_from("<I", data, 16)[0]
    off = 32
    zeroed = 0
    for _ in range(ncmds):
        cmd, size = struct.unpack_from("<II", data, off)
        if cmd == 0x1B:  # LC_UUID
            data[off + 8: off + 24] = b"\x00" * 16
            zeroed += 1
        off += size
    f.seek(0); f.write(data)
print("zeroed" if zeroed else "skip")
PY
)
if [ "$RESULT" = "zeroed" ]; then
  codesign --remove-signature "$BIN" 2>/dev/null || true
  codesign -s - --force "$BIN"
  echo "normalize-macho: normalized $BIN"
else
  echo "normalize-macho: not a 64-bit Mach-O, no-op: $BIN"
fi
