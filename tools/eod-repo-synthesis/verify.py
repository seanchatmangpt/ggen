#!/usr/bin/env python3
"""Execute the end-of-day synthesis verifier across subprocess and filesystem boundaries."""

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any

REPORT_SCHEMA = "ggen.eod-repo-synthesis-verifier-report.v1"


class VerificationError(RuntimeError):
    pass


def canonical_json(value: Any) -> bytes:
    return (json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False) + "\n").encode("utf-8")


def sha256_file(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def require(condition: bool, message: str) -> None:
    if not condition:
        raise VerificationError(message)


def run(command: list[str], *, expected_exit: int = 0) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(command, text=True, capture_output=True, check=False)
    require(
        result.returncode == expected_exit,
        f"command exit mismatch: expected {expected_exit}, observed {result.returncode}; command={command}; stdout={result.stdout}; stderr={result.stderr}",
    )
    return result


def tree_bytes(root: Path) -> dict[str, bytes]:
    return {
        path.relative_to(root).as_posix(): path.read_bytes()
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


def verify_manifest(bundle: Path) -> dict[str, Any]:
    manifest = json.loads((bundle / "manifest.json").read_text(encoding="utf-8"))
    require(manifest["schema"] == "ggen.eod-repo-synthesis-manifest.v1", "manifest schema drift")
    require(manifest["authority"] == "intent_only", "manifest authority drift")
    require(manifest["direct_actuation"] is False, "manifest acquired direct actuation")
    require(manifest["standing"] == "UNKNOWN", "manufacturer self-promoted standing")
    require(manifest["repository_count"] == 5, "admitted repository count drift")
    require(manifest["candidate_target_count"] == 5, "every admitted repository must receive a bounded candidate intent")
    require(manifest["skipped_target_count"] == 0, "unexpected skipped target")

    digest_rows: list[dict[str, str]] = []
    for entry in manifest["files"]:
        path = bundle / entry["path"]
        require(path.is_file(), f"manifest output missing: {entry['path']}")
        observed = sha256_file(path)
        require(observed == entry["sha256"], f"manifest digest mismatch: {entry['path']}")
        digest_rows.append({"path": entry["path"], "sha256": observed})
    observed_bundle = hashlib.sha256(canonical_json(sorted(digest_rows, key=lambda item: item["path"]))).hexdigest()
    require(observed_bundle == manifest["bundle_sha256"], "bundle subject digest mismatch")

    for target in manifest["targets"]:
        intent_path = bundle / target["intent_json"]
        markdown_path = bundle / target["intent_markdown"]
        intent = json.loads(intent_path.read_text(encoding="utf-8"))
        require(markdown_path.read_text(encoding="utf-8") == intent["body_markdown"], f"markdown projection drift for {target['repository']}")
        require(intent["target"]["repository"] == target["repository"], "target identity mismatch")
        require(intent["authority"] == "intent_only", "intent authority drift")
        require(intent["direct_actuation"] is False, "intent acquired direct actuation")
        require(intent["standing"] == "UNKNOWN", "intent self-promoted standing")
        require(intent["disposition"] == "candidate", f"unexpected target disposition for {target['repository']}")
        require(len(intent["candidates"]) == target["candidate_count"], "candidate count drift")
        require(bool(intent["candidates"]), f"candidate intent empty for {target['repository']}")
        for candidate in intent["candidates"]:
            require(candidate["source_repository"] != target["repository"], "self-source crossed the synthesis fence")
            require(len(candidate["source_head_sha"]) == 40, "source head identity malformed")
            require(len(candidate["blob_sha"]) == 40, "source blob identity malformed")
            require(bool(candidate["capabilities"]), "candidate lacks admitted capability intersection")
    return manifest


def expect_refusal(script: Path, observation: dict[str, Any], root: Path, code: str, name: str) -> dict[str, Any]:
    input_path = root / f"{name}.json"
    output_path = root / f"{name}-output"
    input_path.write_bytes(canonical_json(observation))
    result = run(
        [sys.executable, str(script), "--observation", str(input_path), "--output-dir", str(output_path)],
        expected_exit=2,
    )
    require(code in result.stderr, f"typed refusal {code} absent for {name}: {result.stderr}")
    require(not output_path.exists(), f"refused run published output for {name}")
    return {"name": name, "code": code, "exit_code": result.returncode, "stderr_sha256": hashlib.sha256(result.stderr.encode()).hexdigest()}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--evidence-dir", type=Path, required=True)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    root = args.root.resolve()
    tool_dir = root / "tools" / "eod-repo-synthesis"
    script = tool_dir / "synthesize.py"
    fixture = tool_dir / "evidence" / "2026-07-31.observation.json"
    require(script.is_file(), f"manufacturer missing: {script}")
    require(fixture.is_file(), f"admitted observation missing: {fixture}")
    observation = json.loads(fixture.read_text(encoding="utf-8"))

    checks: list[dict[str, Any]] = []
    with tempfile.TemporaryDirectory(prefix="ggen-eod-synthesis-") as raw_temp:
        temporary = Path(raw_temp)
        first = temporary / "first"
        second = temporary / "second"
        first_run = run([sys.executable, str(script), "--observation", str(fixture), "--output-dir", str(first)])
        second_run = run([sys.executable, str(script), "--observation", str(fixture), "--output-dir", str(second)])
        require(tree_bytes(first) == tree_bytes(second), "byte-identical replay failed")
        first_manifest = verify_manifest(first)
        second_manifest = verify_manifest(second)
        require(first_manifest == second_manifest, "manifest replay drift")
        checks.append(
            {
                "name": "subprocess-filesystem-replay",
                "status": "PASS",
                "first_exit": first_run.returncode,
                "second_exit": second_run.returncode,
                "bundle_sha256": first_manifest["bundle_sha256"],
                "file_count": len(tree_bytes(first)),
            }
        )

        changed = json.loads(json.dumps(observation))
        original_blob = changed["repositories"][1]["work"][0]["artifacts"][0]["blob_sha"]
        changed_blob = "f" * 40
        require(original_blob != changed_blob, "tamper control did not change the source identity")
        changed["repositories"][1]["work"][0]["artifacts"][0]["blob_sha"] = changed_blob
        changed_input = temporary / "changed.json"
        changed_output = temporary / "changed-output"
        changed_input.write_bytes(canonical_json(changed))
        changed_run = run([sys.executable, str(script), "--observation", str(changed_input), "--output-dir", str(changed_output)])
        changed_manifest = verify_manifest(changed_output)
        require(changed_manifest["bundle_sha256"] != first_manifest["bundle_sha256"], "source identity mutation did not change bundle identity")
        changed_bytes = b"".join(tree_bytes(changed_output).values())
        require(changed_blob.encode() in changed_bytes, "changed source identity did not propagate into consequences")
        checks.append(
            {
                "name": "source-identity-causality",
                "status": "PASS",
                "exit_code": changed_run.returncode,
                "before_bundle_sha256": first_manifest["bundle_sha256"],
                "after_bundle_sha256": changed_manifest["bundle_sha256"],
            }
        )

        duplicate = json.loads(json.dumps(observation))
        duplicate["repositories"].append(json.loads(json.dumps(duplicate["repositories"][0])))
        checks.append(expect_refusal(script, duplicate, temporary, "EOD-SYNTH-002", "duplicate-repository"))

        escaping = json.loads(json.dumps(observation))
        escaping["repositories"][0]["work"][0]["artifacts"][0]["path"] = "../outside"
        checks.append(expect_refusal(script, escaping, temporary, "EOD-SYNTH-006", "path-escape"))

        actuating = json.loads(json.dumps(observation))
        actuating["direct_actuation"] = True
        checks.append(expect_refusal(script, actuating, temporary, "EOD-SYNTH-007", "direct-actuation"))

        occupied = temporary / "occupied"
        occupied.mkdir()
        (occupied / "existing").write_text("preserve", encoding="utf-8")
        occupied_result = run(
            [sys.executable, str(script), "--observation", str(fixture), "--output-dir", str(occupied)],
            expected_exit=2,
        )
        require("EOD-SYNTH-005" in occupied_result.stderr, "occupied-output refusal missing")
        require((occupied / "existing").read_text(encoding="utf-8") == "preserve", "occupied output was mutated")
        checks.append(
            {
                "name": "occupied-output",
                "status": "PASS",
                "code": "EOD-SYNTH-005",
                "exit_code": occupied_result.returncode,
            }
        )

    report = {
        "schema": REPORT_SCHEMA,
        "standing": "PARTIAL_ALIVE",
        "subject": {
            "observation": fixture.relative_to(root).as_posix(),
            "observation_sha256": sha256_file(fixture),
            "manufacturer": script.relative_to(root).as_posix(),
            "manufacturer_sha256": sha256_file(script),
        },
        "checks": checks,
        "surfaces": {
            "execution": "real Python subprocess invocations",
            "state": "real temporary filesystem consequences and immutable occupied-state refusal",
            "process": "machine-readable command exits and typed refusal codes",
            "causality": "one immutable source blob mutation changes the complete consequence bundle identity",
        },
        "claim_boundary": {
            "github_writes": "NOT_EXECUTED",
            "target_repository_builds": "NOT_EXECUTED",
            "blake3_receipt": "REQUIRED_BEFORE_ACTUATION",
            "external_replay": "UNKNOWN",
            "aggregate_target_standing": "UNKNOWN",
        },
        "failures": 0,
    }
    args.evidence_dir.mkdir(parents=True, exist_ok=True)
    report_path = args.evidence_dir / "verifier-report.json"
    report_path.write_bytes(canonical_json(report))
    print(json.dumps(report, sort_keys=True))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except VerificationError as error:
        print(f"EOD-SYNTH-VERIFY-001: {error}", file=sys.stderr)
        raise SystemExit(1)
