#!/usr/bin/env python3
"""豊田コード生産方式 v26.7.19 製品ライフサイクル実行器。

標準ライブラリだけで動作する。対象一覧は rustc --print target-list から取得し、
公式スナップショットに存在しない対象は第三階層として扱う。
"""
from __future__ import annotations

import argparse
import dataclasses
import datetime as dt
import hashlib
import json
import os
import platform
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import time
import zipfile
from pathlib import Path
from typing import Any, Iterable, Sequence

ROOT = Path(__file__).resolve().parents[1]
TARGET_DIR = ROOT / "target"
DIST_DIR = ROOT / "dist"
RECEIPT_DIR = ROOT / "receipts"
STATE_DIR = ROOT / ".lifecycle"
SNAPSHOT_PATH = ROOT / "targets" / "official-platform-support-1.97.1.json"
OVERRIDES_PATH = ROOT / "targets" / "family-overrides.json"
RELEASE_PATH = ROOT / "release.json"
SCHEMA_VERSION = "1"


class LifecycleError(RuntimeError):
    """A lifecycle operation could not preserve standing."""


@dataclasses.dataclass(frozen=True)
class CommandResult:
    command: list[str]
    returncode: int
    stdout: str
    stderr: str
    duration_ms: int

    @property
    def succeeded(self) -> bool:
        return self.returncode == 0


@dataclasses.dataclass(frozen=True)
class BuildOutcome:
    target: str
    product: str
    tier: int
    family: str
    status: str
    command: list[str]
    duration_ms: int
    receipt: str
    detail: str


def utc_now() -> str:
    return dt.datetime.now(dt.timezone.utc).replace(microsecond=0).isoformat()


def ensure_dirs() -> None:
    for path in (DIST_DIR, RECEIPT_DIR, STATE_DIR):
        path.mkdir(parents=True, exist_ok=True)


def load_json(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text(encoding="utf-8"))


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    os.replace(temporary, path)


def command_exists(name: str) -> bool:
    return shutil.which(name) is not None


def run(
    command: Sequence[str],
    *,
    cwd: Path = ROOT,
    env: dict[str, str] | None = None,
    check: bool = False,
    timeout: int | None = None,
) -> CommandResult:
    started = time.monotonic_ns()
    merged = os.environ.copy()
    if env:
        merged.update(env)
    try:
        completed = subprocess.run(
            list(command),
            cwd=cwd,
            env=merged,
            text=True,
            encoding="utf-8",
            errors="replace",
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout,
            check=False,
        )
        result = CommandResult(
            command=list(command),
            returncode=completed.returncode,
            stdout=completed.stdout,
            stderr=completed.stderr,
            duration_ms=(time.monotonic_ns() - started) // 1_000_000,
        )
    except FileNotFoundError as exc:
        result = CommandResult(
            command=list(command),
            returncode=127,
            stdout="",
            stderr=str(exc),
            duration_ms=(time.monotonic_ns() - started) // 1_000_000,
        )
    except subprocess.TimeoutExpired as exc:
        stdout = exc.stdout if isinstance(exc.stdout, str) else ""
        stderr = exc.stderr if isinstance(exc.stderr, str) else ""
        result = CommandResult(
            command=list(command),
            returncode=124,
            stdout=stdout,
            stderr=stderr + "\n時間上限に到達しました",
            duration_ms=(time.monotonic_ns() - started) // 1_000_000,
        )
    if check and not result.succeeded:
        raise LifecycleError(
            f"命令が失敗しました: {' '.join(result.command)}\n{result.stderr.strip()}"
        )
    return result


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def source_files() -> list[Path]:
    excluded = {"target", "dist", "receipts", "evidence", ".lifecycle", ".git", "__pycache__"}
    files: list[Path] = []
    for path in ROOT.rglob("*"):
        if not path.is_file():
            continue
        if any(part in excluded for part in path.relative_to(ROOT).parts):
            continue
        if path.suffix in {".pyc", ".pyo"} or path.name == "SOURCE_MANIFEST.sha256":
            continue
        files.append(path)
    return sorted(files)


def tree_digest(files: Iterable[Path] | None = None) -> str:
    digest = hashlib.sha256()
    for path in files or source_files():
        relative = path.relative_to(ROOT).as_posix().encode("utf-8")
        digest.update(len(relative).to_bytes(4, "big"))
        digest.update(relative)
        data_digest = bytes.fromhex(sha256_file(path))
        digest.update(data_digest)
    return digest.hexdigest()


def tool_version(command: Sequence[str]) -> str | None:
    result = run(command)
    if not result.succeeded:
        return None
    text = (result.stdout or result.stderr).strip()
    return text.splitlines()[0] if text else None


def host_target() -> str | None:
    if not command_exists("rustc"):
        return None
    result = run(["rustc", "-vV"])
    if not result.succeeded:
        return None
    for line in result.stdout.splitlines():
        if line.startswith("host: "):
            return line.removeprefix("host: ").strip()
    return None


def official_records() -> dict[str, dict[str, Any]]:
    snapshot = load_json(SNAPSHOT_PATH)
    return {item["target"]: item for item in snapshot["targets"]}


def family_for_target(target: str) -> str:
    families = load_json(OVERRIDES_PATH)["families"]
    for family, config in families.items():
        if family == "other":
            continue
        if any(token in target for token in config["match"]):
            return family
    return "other"


def target_products(target: str, std_kind: str) -> list[str]:
    family = family_for_target(target)
    products = ["core"]
    if family == "wasm":
        products.append("wasm")
    if std_kind == "full" and family not in {"embedded", "gpu", "uefi", "wasm"}:
        products.extend(["std", "ffi"])
    if std_kind == "full" and family in {
        "apple", "android", "windows-msvc", "windows-gnu", "linux-gnu", "linux-musl", "bsd", "solaris", "ohos", "fuchsia", "qnx", "vxworks", "other"
    }:
        products.append("cli")
    return products


def parse_cfg(target: str) -> dict[str, Any]:
    if not command_exists("rustc"):
        return {}
    result = run(["rustc", "--print", "cfg", "--target", target], timeout=30)
    if not result.succeeded:
        return {"cfg_error": result.stderr.strip()}
    cfg: dict[str, Any] = {}
    flags: list[str] = []
    for line in result.stdout.splitlines():
        line = line.strip()
        if not line:
            continue
        match = re.fullmatch(r'([A-Za-z0-9_]+)="(.*)"', line)
        if match:
            key, value = match.groups()
            if key in cfg:
                current = cfg[key]
                if not isinstance(current, list):
                    current = [current]
                current.append(value)
                cfg[key] = current
            else:
                cfg[key] = value
        else:
            flags.append(line)
    if flags:
        cfg["flags"] = flags
    return cfg


def discover_targets() -> dict[str, Any]:
    records = official_records()
    source = "official-snapshot"
    targets: list[str]
    if command_exists("rustc"):
        result = run(["rustc", "--print", "target-list"], timeout=60)
        if result.succeeded:
            targets = sorted({line.strip() for line in result.stdout.splitlines() if line.strip()})
            source = "rustc --print target-list"
        else:
            targets = sorted(records)
    else:
        targets = sorted(records)

    matrix: list[dict[str, Any]] = []
    for target in targets:
        official = records.get(target, {})
        tier = int(official.get("tier", 3))
        std_kind = str(official.get("standard_library", "unknown"))
        family = family_for_target(target)
        matrix.append(
            {
                "target": target,
                "tier": tier,
                "host_tools": bool(official.get("host_tools", False)),
                "standard_library": std_kind,
                "family": family,
                "products": target_products(target, std_kind),
                "cfg": parse_cfg(target) if command_exists("rustc") else {},
            }
        )
    return {
        "schema": SCHEMA_VERSION,
        "generated_at": utc_now(),
        "rustc": tool_version(["rustc", "--version", "--verbose"]),
        "host": host_target(),
        "source": source,
        "targets": matrix,
    }


def receipt_path(step: str, suffix: str = "json") -> Path:
    safe = re.sub(r"[^0-9A-Za-z_.-]+", "-", step).strip("-")
    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    return RECEIPT_DIR / f"{stamp}-{safe}.{suffix}"


def write_step_receipt(step: str, payload: dict[str, Any]) -> Path:
    ensure_dirs()
    path = receipt_path(step)
    value = {
        "schema": SCHEMA_VERSION,
        "step": step,
        "recorded_at": utc_now(),
        "source_digest": tree_digest(),
        "release": load_json(RELEASE_PATH),
        **payload,
    }
    write_json(path, value)
    return path


def doctor() -> int:
    ensure_dirs()
    tools = {
        "python": sys.version.split()[0],
        "platform": platform.platform(),
        "cargo": tool_version(["cargo", "--version"]),
        "rustc": tool_version(["rustc", "--version", "--verbose"]),
        "rustup": tool_version(["rustup", "--version"]),
        "rustfmt": tool_version(["rustfmt", "--version"]),
        "clippy": tool_version(["cargo", "clippy", "--version"]),
        "git": tool_version(["git", "--version"]),
        "cmake": tool_version(["cmake", "--version"]),
        "ninja": tool_version(["ninja", "--version"]),
        "zig": tool_version(["zig", "version"]),
        "cross": tool_version(["cross", "--version"]),
        "wasmtime": tool_version(["wasmtime", "--version"]),
        "node": tool_version(["node", "--version"]),
        "qemu": tool_version(["qemu-system-x86_64", "--version"]),
        "cosign": tool_version(["cosign", "version"]),
        "minisign": tool_version(["minisign", "-v"]),
        "gpg": tool_version(["gpg", "--version"]),
    }
    missing_required = [name for name in ("cargo", "rustc", "rustup") if tools[name] is None]
    receipt = write_step_receipt(
        "doctor",
        {
            "status": "ready" if not missing_required else "tool-missing",
            "missing_required": missing_required,
            "tools": tools,
        },
    )
    print(json.dumps({"receipt": str(receipt), **tools}, ensure_ascii=False, indent=2))
    return 0 if not missing_required else 2


def structural_verify() -> tuple[bool, list[str]]:
    errors: list[str] = []
    required = [
        ROOT / "Cargo.toml",
        ROOT / "rust-toolchain.toml",
        ROOT / "crates/tcps-core/src/lib.rs",
        ROOT / "crates/tcps-ffi/include/tcps.h",
        ROOT / "tools/lifecycle.py",
        SNAPSHOT_PATH,
    ]
    for path in required:
        if not path.exists():
            errors.append(f"必須ファイルがありません: {path.relative_to(ROOT)}")

    core_sources = sorted((ROOT / "crates/tcps-core/src").glob("*.rs"))
    if not core_sources:
        errors.append("中核Rustファイルがありません")
    japanese = re.compile(r"[\u3040-\u30ff\u3400-\u9fff]")
    forbidden = re.compile(r"\b(TODO|FIXME|HACK|XXX)\b")
    for path in sorted(ROOT.rglob("*.rs")):
        text = path.read_text(encoding="utf-8")
        if forbidden.search(text):
            errors.append(f"未閉鎖印があります: {path.relative_to(ROOT)}")
        if path.is_relative_to(ROOT / "crates/tcps-core") and not japanese.search(text):
            errors.append(f"中核に日本語意味語彙がありません: {path.relative_to(ROOT)}")
        if path.is_relative_to(ROOT / "crates/tcps-core") and re.search(r"\bunsafe\b", text):
            errors.append(f"中核でunsafeを検出しました: {path.relative_to(ROOT)}")
        # Fast balance check, not a Rust parser.
        pairs = [("{", "}"), ("(", ")"), ("[", "]")]
        for opening, closing in pairs:
            if text.count(opening) != text.count(closing):
                errors.append(f"区切り数不一致 {opening}{closing}: {path.relative_to(ROOT)}")

    invariants = {
        "自働化": "src/自働化.rs",
        "後工程引取り": "src/必要時生産.rs",
        "選択と許可の分離": "src/青い川のダム.rs",
        "受領証": "src/受領証.rs",
    }
    for name, relative in invariants.items():
        path = ROOT / "crates/tcps-core" / relative
        if not path.exists():
            errors.append(f"不変条件の実装がありません: {name}")
    return not errors, errors


def verify(*, require_tools: bool = True) -> int:
    ensure_dirs()
    structure_ok, errors = structural_verify()
    commands: list[dict[str, Any]] = []
    if structure_ok and command_exists("cargo"):
        command_list = [
            ["cargo", "fmt", "--all", "--", "--check"],
            ["cargo", "check", "--workspace", "--all-targets"],
            ["cargo", "test", "--workspace", "--all-targets"],
            ["cargo", "clippy", "--workspace", "--all-targets", "--", "-D", "clippy::correctness", "-D", "clippy::suspicious", "-D", "clippy::perf"],
            ["cargo", "doc", "--workspace", "--no-deps"],
        ]
        for command in command_list:
            result = run(command, timeout=1800)
            commands.append(dataclasses.asdict(result))
            if not result.succeeded:
                errors.append(f"検査失敗: {' '.join(command)}")
                break
    elif require_tools:
        errors.append("cargoがないため機械検査を実行できません")

    status = "accepted" if structure_ok and not errors else "refused"
    receipt = write_step_receipt(
        "verify",
        {"status": status, "structure_ok": structure_ok, "errors": errors, "commands": commands},
    )
    print(f"{status}: {receipt}")
    for error in errors:
        print(f"- {error}", file=sys.stderr)
    return 0 if status == "accepted" else 1


def installed_targets() -> dict[str, bool]:
    if not command_exists("rustup"):
        return {}
    result = run(["rustup", "target", "list"])
    if not result.succeeded:
        return {}
    values: dict[str, bool] = {}
    for line in result.stdout.splitlines():
        line = line.strip()
        if not line:
            continue
        target = line.split()[0]
        values[target] = "(installed)" in line
    return values


def ensure_target(target: str, tier: int) -> tuple[bool, str]:
    installed = installed_targets()
    if installed.get(target, False):
        return True, "installed"
    if not command_exists("rustup"):
        return False, "rustup-missing"
    if tier == 3:
        return False, "tier3-requires-build-std"
    result = run(["rustup", "target", "add", target], timeout=900)
    return result.succeeded, result.stderr.strip() or result.stdout.strip()


def classify_failure(result: CommandResult) -> str:
    text = (result.stdout + "\n" + result.stderr).lower()
    if result.returncode == 127:
        return "tool-missing"
    core_missing = [
        "can't find crate for `core`",
        "target may not be installed",
        "does not support target",
        "toolchain does not support target",
        "component 'rust-std' for target",
        "is unavailable",
    ]
    if any(token in text for token in core_missing):
        return "target-unavailable"
    sdk_tokens = [
        "linker `",
        "linker not found",
        "cannot find -l",
        "xcrun",
        "android_ndk_home",
        "ndk",
        "link.exe",
        "visual studio",
        "sysroot",
        "emcc",
    ]
    if any(token in text for token in sdk_tokens):
        return "sdk-missing"
    if result.returncode == 124:
        return "timed-out"
    return "compiler-refused"


def cargo_build_command(target: str, product: str, profile: str, nightly: bool = False) -> list[str]:
    cargo = "cargo"
    prefix = [cargo]
    if nightly:
        prefix = [cargo, "+nightly"]
    profile_args = ["--profile", profile]
    build_std = ["-Z", "build-std=core"] if nightly else []
    if product == "core":
        return prefix + ["build", "-p", "tcps-core", "--lib", "--target", target] + profile_args + build_std
    if product == "std":
        return prefix + ["build", "-p", "tcps-std", "--lib", "--target", target] + profile_args
    if product == "ffi":
        return prefix + ["build", "-p", "tcps-ffi", "--target", target] + profile_args
    if product == "wasm":
        return prefix + ["build", "-p", "tcps-wasm", "--target", target] + profile_args
    if product == "cli":
        return prefix + ["build", "-p", "tcps-cli", "--target", target] + profile_args
    raise LifecycleError(f"未知の製品です: {product}")


def build_target(
    target: str,
    *,
    product: str = "core",
    profile: str = "release",
    allow_nightly: bool = True,
) -> BuildOutcome:
    matrix = discover_targets()
    item = next((entry for entry in matrix["targets"] if entry["target"] == target), None)
    if item is None:
        item = {
            "target": target,
            "tier": 3,
            "family": family_for_target(target),
            "standard_library": "unknown",
            "products": ["core"],
        }
    tier = int(item["tier"])
    family = str(item["family"])

    if not command_exists("cargo"):
        result = CommandResult(["cargo"], 127, "", "cargoがありません", 0)
        status = "tool-missing"
    else:
        ready, detail = ensure_target(target, tier)
        command = cargo_build_command(target, product, profile)
        if ready or target == host_target():
            result = run(command, timeout=3600)
        else:
            result = CommandResult(command, 1, "", detail, 0)

        status = "built" if result.succeeded else classify_failure(result)
        if (
            not result.succeeded
            and product == "core"
            and allow_nightly
            and tier == 3
            and command_exists("rustup")
        ):
            toolchains = run(["rustup", "toolchain", "list"])
            if toolchains.succeeded and "nightly" in toolchains.stdout:
                nightly_command = cargo_build_command(target, product, profile, nightly=True)
                nightly_result = run(nightly_command, timeout=3600)
                if nightly_result.succeeded:
                    result = nightly_result
                    status = "built-nightly-build-std"
                else:
                    result = nightly_result
                    status = classify_failure(result)

    receipt = write_step_receipt(
        f"build-{target}-{product}",
        {
            "status": status,
            "target": target,
            "product": product,
            "tier": tier,
            "family": family,
            "command": result.command,
            "returncode": result.returncode,
            "duration_ms": result.duration_ms,
            "stdout": result.stdout[-20000:],
            "stderr": result.stderr[-20000:],
        },
    )
    return BuildOutcome(
        target=target,
        product=product,
        tier=tier,
        family=family,
        status=status,
        command=result.command,
        duration_ms=result.duration_ms,
        receipt=str(receipt),
        detail=(result.stderr or result.stdout).strip()[-2000:],
    )


def choose_targets(
    matrix: dict[str, Any],
    tiers: set[int],
    family: str | None,
    shard_index: int,
    shard_count: int,
) -> list[dict[str, Any]]:
    selected = [
        item
        for item in matrix["targets"]
        if int(item["tier"]) in tiers and (family is None or item["family"] == family)
    ]
    selected.sort(key=lambda item: item["target"])
    if shard_count < 1 or not 0 <= shard_index < shard_count:
        raise LifecycleError("分割番号が不正です")
    return [item for index, item in enumerate(selected) if index % shard_count == shard_index]


def build_all(
    *,
    tiers: set[int],
    family: str | None,
    product: str,
    profile: str,
    shard_index: int,
    shard_count: int,
    strict_tier3: bool,
) -> int:
    matrix = discover_targets()
    write_json(STATE_DIR / "target-matrix.json", matrix)
    selected = choose_targets(matrix, tiers, family, shard_index, shard_count)
    outcomes: list[dict[str, Any]] = []
    failed_required = False
    for item in selected:
        target = item["target"]
        selected_products = [product] if product != "auto" else item["products"]
        for selected_product in selected_products:
            outcome = build_target(target, product=selected_product, profile=profile)
            outcomes.append(dataclasses.asdict(outcome))
            print(f"{target:42} {selected_product:6} {outcome.status}")
            failed = not outcome.status.startswith("built")
            if failed and (outcome.tier in {1, 2} or strict_tier3):
                failed_required = True
    summary = {
        "status": "refused" if failed_required else "completed",
        "tiers": sorted(tiers),
        "family": family,
        "product": product,
        "shard_index": shard_index,
        "shard_count": shard_count,
        "outcomes": outcomes,
    }
    receipt = write_step_receipt("build-all", summary)
    print(f"受領証: {receipt}")
    return 1 if failed_required else 0



def target_record(target: str) -> dict[str, Any]:
    matrix = discover_targets()
    item = next((entry for entry in matrix["targets"] if entry["target"] == target), None)
    if item is not None:
        return item
    return {
        "target": target,
        "tier": 3,
        "family": family_for_target(target),
        "standard_library": "unknown",
        "products": ["core"],
    }


def runner_for_target(target: str) -> tuple[list[str] | None, str]:
    host = host_target()
    if target == host:
        return [], "native"
    if target.startswith("wasm32-wasip1") and command_exists("wasmtime"):
        return ["wasmtime"], "wasmtime"
    if "windows-gnu" in target and command_exists("wine"):
        return ["wine"], "wine"
    arch = target.split("-", 1)[0]
    qemu_map = {
        "aarch64": "qemu-aarch64",
        "arm": "qemu-arm",
        "armv7": "qemu-arm",
        "i686": "qemu-i386",
        "powerpc": "qemu-ppc",
        "powerpc64": "qemu-ppc64",
        "powerpc64le": "qemu-ppc64le",
        "riscv32": "qemu-riscv32",
        "riscv64gc": "qemu-riscv64",
        "s390x": "qemu-s390x",
        "sparc64": "qemu-sparc64",
        "x86_64": "qemu-x86_64",
    }
    executable = qemu_map.get(arch)
    if executable and command_exists(executable):
        return [executable], executable
    return None, "runtime-unavailable"


def test_target(target: str) -> int:
    item = target_record(target)
    if item.get("standard_library") == "core":
        receipt = write_step_receipt(
            f"test-{target}",
            {
                "status": "not-applicable",
                "target": target,
                "reason": "core-only target requires hardware or target-specific harness",
            },
        )
        print(receipt)
        return 0
    ready, detail = ensure_target(target, int(item["tier"]))
    if not ready and target != host_target():
        receipt = write_step_receipt(
            f"test-{target}",
            {"status": "target-unavailable", "target": target, "reason": detail},
        )
        print(receipt)
        return 1
    runner, runner_name = runner_for_target(target)
    if runner is None:
        receipt = write_step_receipt(
            f"test-{target}",
            {
                "status": "runtime-unavailable",
                "target": target,
                "reason": "build is possible but no approved runner was detected",
            },
        )
        print(receipt)
        return 1
    env: dict[str, str] = {}
    if runner:
        key = "CARGO_TARGET_" + target.upper().replace("-", "_") + "_RUNNER"
        env[key] = " ".join(runner)
    command = ["cargo", "test", "-p", "tcps-core", "--target", target]
    result = run(command, env=env, timeout=3600)
    status = "tested" if result.succeeded else classify_failure(result)
    receipt = write_step_receipt(
        f"test-{target}",
        {
            "status": status,
            "target": target,
            "runner": runner_name,
            "command": command,
            "returncode": result.returncode,
            "duration_ms": result.duration_ms,
            "stdout": result.stdout[-20000:],
            "stderr": result.stderr[-20000:],
        },
    )
    print(receipt)
    return 0 if result.succeeded else 1


def benchmark(iterations: int) -> int:
    if not command_exists("cargo"):
        receipt = write_step_receipt(
            "benchmark",
            {"status": "tool-missing", "reason": "cargo is not installed"},
        )
        print(receipt)
        return 1
    command = [
        "cargo",
        "run",
        "-p",
        "tcps-cli",
        "--bin",
        "tcps-bench",
        "--release",
        "--",
        str(iterations),
    ]
    result = run(command, timeout=3600)
    parsed: dict[str, Any] | None = None
    if result.succeeded:
        for line in reversed(result.stdout.splitlines()):
            try:
                parsed = json.loads(line)
                break
            except json.JSONDecodeError:
                continue
    receipt = write_step_receipt(
        "benchmark",
        {
            "status": "measured" if result.succeeded and parsed else "refused",
            "iterations": iterations,
            "host": host_target(),
            "command": command,
            "duration_ms": result.duration_ms,
            "measurement": parsed,
            "stdout": result.stdout[-20000:],
            "stderr": result.stderr[-20000:],
            "claim_boundary": "This receipt applies only to the named source digest, compiler, host, and invocation.",
        },
    )
    print(receipt)
    return 0 if result.succeeded and parsed else 1

def cargo_artifact_root(target: str, profile: str) -> Path:
    profile_dir = "release" if profile == "release" else profile
    return TARGET_DIR / target / profile_dir


def collect_artifacts(target: str, profile: str) -> list[Path]:
    root = cargo_artifact_root(target, profile)
    if not root.exists():
        return []
    patterns = [
        "*.rlib", "*.a", "*.so", "*.dylib", "*.dll", "*.lib", "*.wasm", "*.exe", "tcps", "tcps-*"
    ]
    found: set[Path] = set()
    for pattern in patterns:
        for path in root.glob(pattern):
            if path.is_file() and not path.name.endswith((".d", ".rmeta")):
                found.add(path)
    deps = root / "deps"
    if deps.exists():
        for pattern in ("libtcps_core-*.rlib", "libtcps_wasm*.wasm", "tcps*.dll", "libtcps_ffi*.so", "libtcps_ffi*.dylib", "libtcps_ffi*.a"):
            found.update(path for path in deps.glob(pattern) if path.is_file())
    return sorted(found)


def package_target(target: str, *, profile: str = "release") -> int:
    artifacts = collect_artifacts(target, profile)
    if not artifacts:
        receipt = write_step_receipt(
            f"package-{target}",
            {"status": "refused", "target": target, "reason": "build-artifacts-not-found"},
        )
        print(f"成果物がありません: {receipt}", file=sys.stderr)
        return 1

    package_root = DIST_DIR / f"tcps-v26.7.19-{target}"
    if package_root.exists():
        shutil.rmtree(package_root)
    (package_root / "lib").mkdir(parents=True)
    (package_root / "include").mkdir()
    (package_root / "meta").mkdir()
    for artifact in artifacts:
        shutil.copy2(artifact, package_root / "lib" / artifact.name)
    shutil.copy2(ROOT / "include/tcps.h", package_root / "include/tcps.h")
    for name in ("README.md", "README_EN.md", "LICENSE-MIT", "LICENSE-APACHE"):
        source = ROOT / name
        if source.exists():
            shutil.copy2(source, package_root / name)

    metadata = {
        "target": target,
        "profile": profile,
        "family": family_for_target(target),
        "artifacts": [
            {
                "path": path.relative_to(package_root).as_posix(),
                "size": path.stat().st_size,
                "sha256": sha256_file(path),
            }
            for path in sorted(package_root.rglob("*"))
            if path.is_file()
        ],
    }
    write_json(package_root / "meta/package.json", metadata)

    zip_path = DIST_DIR / f"tcps-v26.7.19-{target}.zip"
    with zipfile.ZipFile(zip_path, "w", compression=zipfile.ZIP_DEFLATED, compresslevel=9) as archive:
        for path in sorted(package_root.rglob("*")):
            if path.is_file():
                archive.write(path, path.relative_to(DIST_DIR))
    tar_path = DIST_DIR / f"tcps-v26.7.19-{target}.tar.gz"
    with tarfile.open(tar_path, "w:gz", compresslevel=9) as archive:
        archive.add(package_root, arcname=package_root.name)

    receipt = write_step_receipt(
        f"package-{target}",
        {
            "status": "packaged",
            "target": target,
            "zip": str(zip_path.relative_to(ROOT)),
            "zip_sha256": sha256_file(zip_path),
            "tar": str(tar_path.relative_to(ROOT)),
            "tar_sha256": sha256_file(tar_path),
        },
    )
    print(zip_path)
    print(tar_path)
    print(receipt)
    return 0


def cargo_packages() -> list[dict[str, Any]]:
    if command_exists("cargo"):
        result = run(["cargo", "metadata", "--format-version", "1", "--no-deps"], timeout=120)
        if result.succeeded:
            data = json.loads(result.stdout)
            return [
                {
                    "name": package["name"],
                    "version": package["version"],
                    "license": package.get("license"),
                    "manifest_path": str(Path(package["manifest_path"]).relative_to(ROOT)),
                }
                for package in data["packages"]
            ]
    return [
        {"name": "tcps-core", "version": "26.7.19", "license": "MIT OR Apache-2.0", "manifest_path": "crates/tcps-core/Cargo.toml"},
        {"name": "tcps-std", "version": "26.7.19", "license": "MIT OR Apache-2.0", "manifest_path": "crates/tcps-std/Cargo.toml"},
        {"name": "tcps-ffi", "version": "26.7.19", "license": "MIT OR Apache-2.0", "manifest_path": "crates/tcps-ffi/Cargo.toml"},
        {"name": "tcps-wasm", "version": "26.7.19", "license": "MIT OR Apache-2.0", "manifest_path": "crates/tcps-wasm/Cargo.toml"},
        {"name": "tcps-cli", "version": "26.7.19", "license": "MIT OR Apache-2.0", "manifest_path": "crates/tcps-cli/Cargo.toml"},
    ]


def generate_sbom() -> int:
    ensure_dirs()
    packages = cargo_packages()
    serial = f"urn:uuid:{hashlib.sha256(tree_digest().encode()).hexdigest()[:32]}"
    cyclonedx = {
        "bomFormat": "CycloneDX",
        "specVersion": "1.5",
        "serialNumber": serial,
        "version": 1,
        "metadata": {
            "timestamp": utc_now(),
            "component": {
                "type": "application",
                "name": "豊田コード生産方式",
                "version": "26.7.19",
            },
        },
        "components": [
            {
                "type": "library" if package["name"] != "tcps-cli" else "application",
                "name": package["name"],
                "version": package["version"],
                "licenses": [{"expression": package["license"] or "NOASSERTION"}],
                "purl": f"pkg:cargo/{package['name']}@{package['version']}",
            }
            for package in packages
        ],
    }
    spdx = {
        "spdxVersion": "SPDX-2.3",
        "dataLicense": "CC0-1.0",
        "SPDXID": "SPDXRef-DOCUMENT",
        "name": "tcps-v26.7.19",
        "documentNamespace": f"urn:tcps:spdx:{tree_digest()}",
        "creationInfo": {"created": utc_now(), "creators": ["Tool: tools/lifecycle.py"]},
        "packages": [
            {
                "name": package["name"],
                "SPDXID": f"SPDXRef-Package-{package['name']}",
                "versionInfo": package["version"],
                "downloadLocation": "NOASSERTION",
                "licenseConcluded": package["license"] or "NOASSERTION",
                "licenseDeclared": package["license"] or "NOASSERTION",
                "filesAnalyzed": False,
            }
            for package in packages
        ],
    }
    cyclonedx_path = DIST_DIR / "tcps-v26.7.19.cdx.json"
    spdx_path = DIST_DIR / "tcps-v26.7.19.spdx.json"
    write_json(cyclonedx_path, cyclonedx)
    write_json(spdx_path, spdx)
    receipt = write_step_receipt(
        "sbom",
        {
            "status": "generated",
            "cyclonedx": str(cyclonedx_path.relative_to(ROOT)),
            "cyclonedx_sha256": sha256_file(cyclonedx_path),
            "spdx": str(spdx_path.relative_to(ROOT)),
            "spdx_sha256": sha256_file(spdx_path),
        },
    )
    print(cyclonedx_path)
    print(spdx_path)
    print(receipt)
    return 0


def git_information() -> dict[str, Any]:
    if not command_exists("git"):
        return {"available": False}
    top = run(["git", "rev-parse", "--show-toplevel"])
    if not top.succeeded:
        return {"available": True, "repository": False}
    commit = run(["git", "rev-parse", "HEAD"])
    status = run(["git", "status", "--porcelain"])
    return {
        "available": True,
        "repository": True,
        "commit": commit.stdout.strip() if commit.succeeded else None,
        "dirty": bool(status.stdout.strip()) if status.succeeded else None,
    }


def generate_provenance() -> int:
    subjects = []
    for path in sorted(DIST_DIR.glob("*")):
        if path.is_file() and path.name != "tcps-v26.7.19.intoto.jsonl":
            subjects.append({"name": path.name, "digest": {"sha256": sha256_file(path)}})
    statement = {
        "_type": "https://in-toto.io/Statement/v1",
        "subject": subjects,
        "predicateType": "https://slsa.dev/provenance/v1",
        "predicate": {
            "buildDefinition": {
                "buildType": "urn:tcps:rust-lifecycle:v1",
                "externalParameters": load_json(RELEASE_PATH),
                "internalParameters": {
                    "rustc": tool_version(["rustc", "--version", "--verbose"]),
                    "cargo": tool_version(["cargo", "--version"]),
                    "host": host_target(),
                },
                "resolvedDependencies": [
                    {"uri": "file:source-tree", "digest": {"sha256": tree_digest()}}
                ],
            },
            "runDetails": {
                "builder": {"id": "tools/lifecycle.py"},
                "metadata": {
                    "invocationId": hashlib.sha256((tree_digest() + utc_now()).encode()).hexdigest(),
                    "startedOn": utc_now(),
                    "finishedOn": utc_now(),
                },
            },
        },
    }
    path = DIST_DIR / "tcps-v26.7.19.intoto.jsonl"
    path.write_text(json.dumps(statement, ensure_ascii=False, separators=(",", ":")) + "\n", encoding="utf-8")
    receipt = write_step_receipt(
        "provenance",
        {
            "status": "generated",
            "path": str(path.relative_to(ROOT)),
            "sha256": sha256_file(path),
            "git": git_information(),
        },
    )
    print(path)
    print(receipt)
    return 0


def generate_checksums() -> int:
    ensure_dirs()
    lines = []
    for path in sorted(DIST_DIR.glob("*")):
        if path.is_file() and path.name not in {"SHA256SUMS", "SHA256SUMS.sig", "SHA256SUMS.asc"}:
            lines.append(f"{sha256_file(path)}  {path.name}")
    checksum_path = DIST_DIR / "SHA256SUMS"
    checksum_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    receipt = write_step_receipt(
        "checksums",
        {"status": "generated", "path": str(checksum_path.relative_to(ROOT)), "entries": len(lines)},
    )
    print(checksum_path)
    print(receipt)
    return 0


def sign_checksums(method: str, key: str | None) -> int:
    checksum_path = DIST_DIR / "SHA256SUMS"
    if not checksum_path.exists():
        generate_checksums()
    if method == "none":
        receipt = write_step_receipt("sign", {"status": "not-requested", "method": method})
        print(receipt)
        return 0
    if method == "gpg":
        if not command_exists("gpg"):
            raise LifecycleError("gpgがありません")
        command = ["gpg", "--batch", "--yes", "--armor", "--detach-sign", "--output", str(checksum_path) + ".asc"]
        if key:
            command.extend(["--local-user", key])
        command.append(str(checksum_path))
    elif method == "minisign":
        if not command_exists("minisign"):
            raise LifecycleError("minisignがありません")
        if not key:
            raise LifecycleError("minisign秘密鍵を指定してください")
        command = ["minisign", "-S", "-s", key, "-m", str(checksum_path)]
    elif method == "cosign":
        if not command_exists("cosign"):
            raise LifecycleError("cosignがありません")
        command = ["cosign", "sign-blob", "--yes", "--output-signature", str(checksum_path) + ".sig"]
        if key:
            command.extend(["--key", key])
        command.append(str(checksum_path))
    else:
        raise LifecycleError(f"未対応の署名方式です: {method}")
    result = run(command, timeout=600)
    receipt = write_step_receipt(
        "sign",
        {
            "status": "signed" if result.succeeded else "refused",
            "method": method,
            "command": result.command,
            "returncode": result.returncode,
            "stderr": result.stderr,
        },
    )
    print(receipt)
    return 0 if result.succeeded else 1


def verify_release() -> int:
    checksum_path = DIST_DIR / "SHA256SUMS"
    errors: list[str] = []
    if not checksum_path.exists():
        errors.append("SHA256SUMSがありません")
    else:
        for line in checksum_path.read_text(encoding="utf-8").splitlines():
            if not line.strip():
                continue
            expected, name = line.split("  ", 1)
            path = DIST_DIR / name
            if not path.exists():
                errors.append(f"成果物がありません: {name}")
            elif sha256_file(path) != expected:
                errors.append(f"要約不一致: {name}")
    for required in ("tcps-v26.7.19.cdx.json", "tcps-v26.7.19.spdx.json", "tcps-v26.7.19.intoto.jsonl"):
        if not (DIST_DIR / required).exists():
            errors.append(f"供給網成果物がありません: {required}")
    receipt = write_step_receipt(
        "verify-release",
        {"status": "accepted" if not errors else "refused", "errors": errors},
    )
    print(receipt)
    for error in errors:
        print(f"- {error}", file=sys.stderr)
    return 0 if not errors else 1


def release(scope: str, sign_method: str, sign_key: str | None) -> int:
    if verify(require_tools=True) != 0:
        return 1
    matrix = discover_targets()
    write_json(DIST_DIR / "target-matrix.json", matrix)
    targets: list[str]
    if scope == "host":
        host = host_target()
        if not host:
            raise LifecycleError("ホスト対象を取得できません")
        targets = [host]
    elif scope == "tier1":
        targets = [item["target"] for item in matrix["targets"] if item["tier"] == 1]
    elif scope == "tier1-2":
        targets = [item["target"] for item in matrix["targets"] if item["tier"] in {1, 2}]
    elif scope == "all":
        targets = [item["target"] for item in matrix["targets"]]
    else:
        targets = [scope]

    failures = []
    for target in targets:
        outcome = build_target(target, product="core", profile="release")
        print(f"{target}: {outcome.status}")
        if not outcome.status.startswith("built"):
            failures.append(dataclasses.asdict(outcome))
            continue
        package_target(target)

    generate_sbom()
    generate_provenance()
    generate_checksums()
    if sign_method != "none":
        sign_checksums(sign_method, sign_key)
    verification = verify_release()
    receipt = write_step_receipt(
        "release",
        {
            "status": "accepted" if not failures and verification == 0 else "partial",
            "scope": scope,
            "targets": targets,
            "failures": failures,
        },
    )
    print(receipt)
    return 0 if not failures and verification == 0 else 1


def clean() -> int:
    for path in (TARGET_DIR, DIST_DIR, RECEIPT_DIR, STATE_DIR):
        if path.exists():
            shutil.rmtree(path)
    ensure_dirs()
    (DIST_DIR / ".gitkeep").touch()
    (RECEIPT_DIR / ".gitkeep").touch()
    print("清掃しました")
    return 0


def matrix_command(output: str | None) -> int:
    matrix = discover_targets()
    path = Path(output) if output else DIST_DIR / "target-matrix.json"
    if not path.is_absolute():
        path = ROOT / path
    write_json(path, matrix)
    counts: dict[str, int] = {}
    for item in matrix["targets"]:
        key = f"tier-{item['tier']}"
        counts[key] = counts.get(key, 0) + 1
    receipt = write_step_receipt(
        "target-matrix",
        {"status": "generated", "path": str(path), "counts": counts, "source": matrix["source"]},
    )
    print(json.dumps({"path": str(path), "counts": counts, "receipt": str(receipt)}, indent=2))
    return 0


def parse_tiers(value: str) -> set[int]:
    tiers = {int(part.strip()) for part in value.split(",") if part.strip()}
    if not tiers or not tiers.issubset({1, 2, 3}):
        raise argparse.ArgumentTypeError("階層は1,2,3の組合せです")
    return tiers


def parser() -> argparse.ArgumentParser:
    command = argparse.ArgumentParser(description=__doc__)
    sub = command.add_subparsers(dest="command", required=True)

    sub.add_parser("doctor", help="道具とSDKの状態を観察する")
    verify_parser = sub.add_parser("verify", help="ホスト上の完全検査を行う")
    verify_parser.add_argument("--structure-only", action="store_true")

    matrix_parser = sub.add_parser("matrix", help="対象行列を生成する")
    matrix_parser.add_argument("--output")

    build_parser = sub.add_parser("build", help="一つの対象を構築する")
    build_parser.add_argument("--target", required=True)
    build_parser.add_argument("--product", choices=["core", "std", "ffi", "wasm", "cli"], default="core")
    build_parser.add_argument("--profile", default="release")
    build_parser.add_argument("--no-nightly", action="store_true")

    all_parser = sub.add_parser("build-all", help="対象行列を分割構築する")
    all_parser.add_argument("--tiers", type=parse_tiers, default={1, 2})
    all_parser.add_argument("--family")
    all_parser.add_argument("--product", choices=["auto", "core", "std", "ffi", "wasm", "cli"], default="core")
    all_parser.add_argument("--profile", default="release")
    all_parser.add_argument("--shard-index", type=int, default=0)
    all_parser.add_argument("--shard-count", type=int, default=1)
    all_parser.add_argument("--strict-tier3", action="store_true")

    test_parser = sub.add_parser("test", help="対象上で試験を実行する")
    test_parser.add_argument("--target", required=True)

    benchmark_parser = sub.add_parser("benchmark", help="ホスト上で閉路選択を計測する")
    benchmark_parser.add_argument("--iterations", type=int, default=1_000_000)

    package_parser = sub.add_parser("package", help="対象別配布物を作る")
    package_parser.add_argument("--target", required=True)
    package_parser.add_argument("--profile", default="release")

    sub.add_parser("sbom", help="CycloneDXとSPDXを生成する")
    sub.add_parser("provenance", help="in-toto/SLSA由来記録を生成する")
    sub.add_parser("checksums", help="配布物要約を生成する")

    sign_parser = sub.add_parser("sign", help="要約一覧を署名する")
    sign_parser.add_argument("--method", choices=["none", "gpg", "minisign", "cosign"], default="none")
    sign_parser.add_argument("--key")

    release_parser = sub.add_parser("release", help="完全な公開経路を実行する")
    release_parser.add_argument("--scope", default="host", help="host, tier1, tier1-2, all, または対象三つ組")
    release_parser.add_argument("--sign-method", choices=["none", "gpg", "minisign", "cosign"], default="none")
    release_parser.add_argument("--sign-key")

    sub.add_parser("verify-release", help="公開成果物を再検証する")
    sub.add_parser("clean", help="生成物を清掃する")
    return command


def main(argv: Sequence[str] | None = None) -> int:
    ensure_dirs()
    args = parser().parse_args(argv)
    try:
        if args.command == "doctor":
            return doctor()
        if args.command == "verify":
            return verify(require_tools=not args.structure_only)
        if args.command == "matrix":
            return matrix_command(args.output)
        if args.command == "build":
            outcome = build_target(
                args.target,
                product=args.product,
                profile=args.profile,
                allow_nightly=not args.no_nightly,
            )
            print(json.dumps(dataclasses.asdict(outcome), ensure_ascii=False, indent=2))
            return 0 if outcome.status.startswith("built") else 1
        if args.command == "build-all":
            return build_all(
                tiers=args.tiers,
                family=args.family,
                product=args.product,
                profile=args.profile,
                shard_index=args.shard_index,
                shard_count=args.shard_count,
                strict_tier3=args.strict_tier3,
            )
        if args.command == "test":
            return test_target(args.target)
        if args.command == "benchmark":
            return benchmark(args.iterations)
        if args.command == "package":
            return package_target(args.target, profile=args.profile)
        if args.command == "sbom":
            return generate_sbom()
        if args.command == "provenance":
            return generate_provenance()
        if args.command == "checksums":
            return generate_checksums()
        if args.command == "sign":
            return sign_checksums(args.method, args.key)
        if args.command == "release":
            return release(args.scope, args.sign_method, args.sign_key)
        if args.command == "verify-release":
            return verify_release()
        if args.command == "clean":
            return clean()
    except LifecycleError as exc:
        receipt = write_step_receipt(
            f"{args.command}-refused",
            {"status": "refused", "reason": str(exc)},
        )
        print(f"拒否: {exc}\n受領証: {receipt}", file=sys.stderr)
        return 2
    raise AssertionError("unreachable")


if __name__ == "__main__":
    raise SystemExit(main())
