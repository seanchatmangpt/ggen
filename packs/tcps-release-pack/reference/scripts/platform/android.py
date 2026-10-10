#!/usr/bin/env python3
from __future__ import annotations
import os, platform, subprocess, sys
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
ndk = os.environ.get("ANDROID_NDK_HOME") or os.environ.get("ANDROID_NDK_ROOT")
if not ndk:
    raise SystemExit("ANDROID_NDK_HOMEまたはANDROID_NDK_ROOTを設定してください")
host_map = {("Linux", "x86_64"): "linux-x86_64", ("Darwin", "x86_64"): "darwin-x86_64", ("Darwin", "arm64"): "darwin-x86_64", ("Windows", "AMD64"): "windows-x86_64"}
host = host_map.get((platform.system(), platform.machine()))
if not host:
    raise SystemExit(f"未対応NDKホスト: {platform.system()} {platform.machine()}")
bin_dir = Path(ndk) / "toolchains" / "llvm" / "prebuilt" / host / "bin"
api = os.environ.get("TCPS_ANDROID_API", "24")
targets = {
    "aarch64-linux-android": f"aarch64-linux-android{api}-clang",
    "armv7-linux-androideabi": f"armv7a-linux-androideabi{api}-clang",
    "i686-linux-android": f"i686-linux-android{api}-clang",
    "x86_64-linux-android": f"x86_64-linux-android{api}-clang",
}
for target, linker in targets.items():
    key = "CARGO_TARGET_" + target.upper().replace("-", "_") + "_LINKER"
    env = os.environ.copy()
    env[key] = str(bin_dir / linker)
    for product in ("core", "ffi"):
        command = [sys.executable, str(ROOT / "tools/lifecycle.py"), "build", "--target", target, "--product", product]
        code = subprocess.call(command, env=env)
        if code != 0 and product == "core":
            raise SystemExit(code)
