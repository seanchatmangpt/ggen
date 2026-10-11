#!/usr/bin/env python3
"""Create optional platform-native wrappers around already-built TCPS artifacts."""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import tarfile
import tempfile
import zipfile
from pathlib import Path
from typing import Sequence

ROOT = Path(__file__).resolve().parents[1]
TARGET = ROOT / "target"
DIST = ROOT / "dist"


def run(command: list[str]) -> None:
    completed = subprocess.run(command, cwd=ROOT, check=False)
    if completed.returncode != 0:
        raise SystemExit(completed.returncode)


def find_one(patterns: list[str]) -> Path:
    found: list[Path] = []
    for pattern in patterns:
        found.extend(path for path in TARGET.glob(pattern) if path.is_file())
    if not found:
        raise SystemExit(f"No artifact matched: {patterns}")
    return sorted(found)[0]


def npm_package(wasm: Path | None) -> Path:
    wasm = wasm or find_one([
        "wasm32-unknown-unknown/release/tcps_wasm.wasm",
        "wasm32-wasip1/release/tcps_wasm.wasm",
        "wasm32-wasip2/release/tcps_wasm.wasm",
    ])
    root = DIST / "npm" / "tcps-core"
    if root.exists():
        shutil.rmtree(root)
    root.mkdir(parents=True)
    shutil.copy2(wasm, root / "tcps_wasm.wasm")
    shutil.copy2(ROOT / "packaging/npm/package.json.in", root / "package.json")
    shutil.copy2(ROOT / "README_EN.md", root / "README.md")
    output = DIST / "tcps-core-26.7.19-npm.tgz"
    with tarfile.open(output, "w:gz") as archive:
        archive.add(root, arcname="package")
    return output


def nuget_package() -> Path:
    root = DIST / "nuget" / "TCPS.Native.26.7.19"
    if root.exists():
        shutil.rmtree(root)
    (root / "build/native/include").mkdir(parents=True)
    (root / "runtimes").mkdir()
    shutil.copy2(ROOT / "packaging/nuget/tcps.nuspec.in", root / "TCPS.Native.nuspec")
    shutil.copy2(ROOT / "include/tcps.h", root / "build/native/include/tcps.h")
    target_map = {
        "win-x64": "x86_64-pc-windows-msvc",
        "win-x86": "i686-pc-windows-msvc",
        "win-arm64": "aarch64-pc-windows-msvc",
    }
    copied = 0
    for runtime, target in target_map.items():
        candidates = list((TARGET / target / "release").glob("tcps_ffi.*")) + list((TARGET / target / "release").glob("*tcps_ffi*"))
        for artifact in candidates:
            if artifact.suffix.lower() in {".dll", ".lib"} and artifact.is_file():
                destination = root / "runtimes" / runtime / "native"
                destination.mkdir(parents=True, exist_ok=True)
                shutil.copy2(artifact, destination / artifact.name)
                copied += 1
    if copied == 0:
        raise SystemExit("No Windows FFI artifacts were found")
    output = DIST / "TCPS.Native.26.7.19.nupkg"
    with zipfile.ZipFile(output, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        for path in sorted(root.rglob("*")):
            if path.is_file():
                archive.write(path, path.relative_to(root))
    return output


def aar_package() -> Path:
    root = DIST / "android" / "tcps"
    if root.exists():
        shutil.rmtree(root)
    root.mkdir(parents=True)
    shutil.copy2(ROOT / "packaging/android/AndroidManifest.xml", root / "AndroidManifest.xml")
    shutil.copy2(ROOT / "include/tcps.h", root / "tcps.h")
    target_map = {
        "arm64-v8a": "aarch64-linux-android",
        "armeabi-v7a": "armv7-linux-androideabi",
        "x86": "i686-linux-android",
        "x86_64": "x86_64-linux-android",
    }
    copied = 0
    for abi, target in target_map.items():
        candidates = list((TARGET / target / "release").glob("libtcps_ffi.so"))
        for artifact in candidates:
            destination = root / "jni" / abi
            destination.mkdir(parents=True, exist_ok=True)
            shutil.copy2(artifact, destination / "libtcps_ffi.so")
            copied += 1
    if copied == 0:
        raise SystemExit("No Android FFI shared libraries were found")
    output = DIST / "tcps-26.7.19.aar"
    with zipfile.ZipFile(output, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        for path in sorted(root.rglob("*")):
            if path.is_file():
                archive.write(path, path.relative_to(root))
    return output


def xcframework() -> Path:
    if shutil.which("xcodebuild") is None:
        raise SystemExit("xcodebuild is required")
    pairs: list[tuple[Path, Path]] = []
    for target in [
        "aarch64-apple-darwin",
        "x86_64-apple-darwin",
        "aarch64-apple-ios",
        "aarch64-apple-ios-sim",
        "x86_64-apple-ios",
    ]:
        lib = TARGET / target / "release" / "libtcps_ffi.a"
        if lib.exists():
            pairs.append((lib, ROOT / "include"))
    if not pairs:
        raise SystemExit("No Apple static libraries were found")
    output = DIST / "TCPS.xcframework"
    if output.exists():
        shutil.rmtree(output)
    command = ["xcodebuild", "-create-xcframework"]
    for lib, headers in pairs:
        command.extend(["-library", str(lib), "-headers", str(headers)])
    command.extend(["-output", str(output)])
    run(command)
    return output


def deb_package(target: str, architecture: str) -> Path:
    if shutil.which("dpkg-deb") is None:
        raise SystemExit("dpkg-deb is required")
    root = DIST / "debian" / f"tcps-{architecture}"
    if root.exists():
        shutil.rmtree(root)
    (root / "DEBIAN").mkdir(parents=True)
    (root / "usr/bin").mkdir(parents=True)
    (root / "usr/include").mkdir(parents=True)
    (root / "usr/lib").mkdir(parents=True)
    control = (ROOT / "packaging/deb/control.in").read_text(encoding="utf-8").replace("@ARCH@", architecture)
    (root / "DEBIAN/control").write_text(control, encoding="utf-8")
    binary = TARGET / target / "release" / "tcps"
    if not binary.exists():
        raise SystemExit(f"CLI artifact missing: {binary}")
    shutil.copy2(binary, root / "usr/bin/tcps")
    shutil.copy2(ROOT / "include/tcps.h", root / "usr/include/tcps.h")
    for artifact in (TARGET / target / "release").glob("libtcps_ffi.*"):
        if artifact.is_file():
            shutil.copy2(artifact, root / "usr/lib" / artifact.name)
    output = DIST / f"tcps_26.7.19_{architecture}.deb"
    run(["dpkg-deb", "--build", "--root-owner-group", str(root), str(output)])
    return output


def parser() -> argparse.ArgumentParser:
    command = argparse.ArgumentParser()
    sub = command.add_subparsers(dest="command", required=True)
    npm = sub.add_parser("npm")
    npm.add_argument("--wasm", type=Path)
    sub.add_parser("nuget")
    sub.add_parser("aar")
    sub.add_parser("xcframework")
    deb = sub.add_parser("deb")
    deb.add_argument("--target", default="x86_64-unknown-linux-gnu")
    deb.add_argument("--architecture", default="amd64")
    return command


def main(argv: Sequence[str] | None = None) -> int:
    DIST.mkdir(parents=True, exist_ok=True)
    args = parser().parse_args(argv)
    if args.command == "npm":
        output = npm_package(args.wasm)
    elif args.command == "nuget":
        output = nuget_package()
    elif args.command == "aar":
        output = aar_package()
    elif args.command == "xcframework":
        output = xcframework()
    elif args.command == "deb":
        output = deb_package(args.target, args.architecture)
    else:
        raise AssertionError("unreachable")
    print(output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
