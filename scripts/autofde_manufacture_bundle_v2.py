#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path, PurePosixPath

REQ_SCHEMA = "autofde.engineering-requirement/1"
ADM_SCHEMA = "autofde.lab-admission/1"
PAYLOAD_SCHEMA = "autofde.compiled-capability/1"
REQUEST_SCHEMA = "autofde.manufacture-request/1"
MANIFEST_SCHEMA = "autofde.capability-bundle-manifest/2"
RECEIPT_SCHEMA = "autofde.manufacture-receipt/2"
VALIDATOR = "ggen:autofde-capability-bundle/2"
REQUIRED_COURTS = (
    "artifact_set_integrity",
    "authority_non_escalation",
    "manifest_binding",
    "provenance_binding",
    "receipt_self_integrity",
    "request_binding",
)


def canonical_bytes(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()


def sha256_json(value):
    return "sha256:" + hashlib.sha256(canonical_bytes(value)).hexdigest()


def raw_sha256(value):
    return hashlib.sha256(canonical_bytes(value)).hexdigest()


def require_git_sha(value, refusal):
    if not isinstance(value, str) or len(value) != 40 or any(ch not in "0123456789abcdef" for ch in value):
        raise ValueError(refusal)


def require_sha256(value, refusal):
    if not isinstance(value, str) or len(value) != 64 or any(ch not in "0123456789abcdef" for ch in value):
        raise ValueError(refusal)


def require_portable_path(value):
    if not isinstance(value, str) or not value or value.startswith(("/", "~")) or "\\" in value:
        raise ValueError("REFUSED:ARTIFACT_PATH_INVALID")
    path = PurePosixPath(value)
    if any(part in {"", ".", ".."} for part in path.parts):
        raise ValueError("REFUSED:ARTIFACT_PATH_INVALID")
    return path.as_posix()


def receipt_digest(receipt):
    unsigned = dict(receipt)
    unsigned.pop("receipt_digest", None)
    return sha256_json(unsigned)


def artifact_set_digest(manifest):
    rows = manifest.get("artifacts")
    if not isinstance(rows, list) or not rows:
        raise ValueError("REFUSED:MANIFEST_ARTIFACTS_INVALID")
    normalized = []
    seen = set()
    for row in rows:
        if not isinstance(row, dict):
            raise ValueError("REFUSED:MANIFEST_ARTIFACT_INVALID")
        path = require_portable_path(row.get("path"))
        digest = row.get("sha256")
        require_sha256(digest, "REFUSED:ARTIFACT_DIGEST_INVALID")
        if path in seen:
            raise ValueError("REFUSED:DUPLICATE_ARTIFACT_PATH")
        seen.add(path)
        normalized.append({"path": path, "sha256": digest})
    return sha256_json(sorted(normalized, key=lambda row: row["path"]))


def seal_manifest(request, manifest, *, ggen_revision, lab_revision):
    if not isinstance(request, dict) or request.get("schema") != REQUEST_SCHEMA:
        raise ValueError("REFUSED:REQUEST_SCHEMA_INVALID")
    if not isinstance(manifest, dict) or manifest.get("schema") != MANIFEST_SCHEMA:
        raise ValueError("REFUSED:MANIFEST_SCHEMA_INVALID")
    require_git_sha(ggen_revision, "REFUSED:GGEN_REVISION_INVALID")
    require_git_sha(lab_revision, "REFUSED:LAB_REVISION_INVALID")
    source = request.get("source", {})
    manufacturer = request.get("manufacturer", {})
    authority = request.get("authority", {})
    if source.get("lab_revision") != lab_revision:
        raise ValueError("REFUSED:LAB_REVISION_DRIFT")
    if manufacturer.get("revision") != ggen_revision:
        raise ValueError("REFUSED:GGEN_REVISION_DRIFT")
    if authority.get("mode") != "external-only" or authority.get("do_authority") is not False:
        raise ValueError("REFUSED:REQUEST_AUTHORITY_ESCALATION")
    request_id = raw_sha256(request)
    if manifest.get("request_id") != request_id:
        raise ValueError("REFUSED:MANIFEST_REQUEST_ID_DRIFT")
    if manifest.get("lab_revision") != lab_revision or manifest.get("ggen_revision") != ggen_revision:
        raise ValueError("REFUSED:MANIFEST_PROVENANCE_DRIFT")
    receipt = {
        "schema": RECEIPT_SCHEMA,
        "standing": "ALIVE",
        "authority_class": "CONSTRUCT",
        "do_authority": False,
        "validator": VALIDATOR,
        "request_id": request_id,
        "request_digest": sha256_json(request),
        "manifest_digest": sha256_json(manifest),
        "artifact_set_digest": artifact_set_digest(manifest),
        "lab_revision": lab_revision,
        "ggen_revision": ggen_revision,
        "courts": list(REQUIRED_COURTS),
    }
    receipt["receipt_digest"] = receipt_digest(receipt)
    verify_manifest_receipt(request, manifest, receipt, expected_ggen_revision=ggen_revision, expected_lab_revision=lab_revision)
    return receipt


def verify_manifest_receipt(request, manifest, receipt, *, expected_ggen_revision=None, expected_lab_revision=None):
    if receipt.get("schema") != RECEIPT_SCHEMA:
        raise ValueError("REFUSED:RECEIPT_SCHEMA_INVALID")
    if receipt.get("standing") != "ALIVE":
        raise ValueError("REFUSED:MANUFACTURER_NOT_ALIVE")
    if receipt.get("authority_class") != "CONSTRUCT" or receipt.get("do_authority") is not False:
        raise ValueError("REFUSED:RECEIPT_AUTHORITY_ESCALATION")
    if receipt.get("validator") != VALIDATOR:
        raise ValueError("REFUSED:VALIDATOR_IDENTITY_DRIFT")
    require_git_sha(receipt.get("ggen_revision"), "REFUSED:GGEN_REVISION_INVALID")
    require_git_sha(receipt.get("lab_revision"), "REFUSED:LAB_REVISION_INVALID")
    if expected_ggen_revision is not None and receipt.get("ggen_revision") != expected_ggen_revision:
        raise ValueError("REFUSED:GGEN_REVISION_DRIFT")
    if expected_lab_revision is not None and receipt.get("lab_revision") != expected_lab_revision:
        raise ValueError("REFUSED:LAB_REVISION_DRIFT")
    if receipt.get("request_id") != raw_sha256(request) or receipt.get("request_digest") != sha256_json(request):
        raise ValueError("REFUSED:REQUEST_BINDING_DRIFT")
    if receipt.get("manifest_digest") != sha256_json(manifest):
        raise ValueError("REFUSED:MANIFEST_BINDING_DRIFT")
    if receipt.get("artifact_set_digest") != artifact_set_digest(manifest):
        raise ValueError("REFUSED:ARTIFACT_SET_BINDING_DRIFT")
    courts = receipt.get("courts")
    if not isinstance(courts, list) or tuple(sorted(courts)) != tuple(sorted(REQUIRED_COURTS)):
        raise ValueError("REFUSED:MANUFACTURE_COURTS_INCOMPLETE")
    if receipt.get("receipt_digest") != receipt_digest(receipt):
        raise ValueError("REFUSED:RECEIPT_DIGEST_MISMATCH")
    return True


def validate_inputs(requirement, admission):
    if requirement.get("schema") != REQ_SCHEMA or requirement.get("standing") != "BLOCKED:CAPABILITY_ABSENT":
        raise ValueError("REFUSED:REQUIREMENT_NOT_ADMITTED")
    if requirement.get("authority_class") != "CONSTRUCT":
        raise ValueError("REFUSED:REQUIREMENT_AUTHORITY_ESCALATION")
    if admission.get("schema") != ADM_SCHEMA or admission.get("standing") != "ALIVE":
        raise ValueError("REFUSED:LAB_ADMISSION_INVALID")
    if admission.get("do_authority") is not False or admission.get("authority_class") != "CONSTRUCT":
        raise ValueError("REFUSED:LAB_AUTHORITY_ESCALATION")
    if admission.get("requirement_id") != requirement.get("requirement_id"):
        raise ValueError("REFUSED:REQUIREMENT_ID_DRIFT")
    if admission.get("requirement_digest") != sha256_json(requirement):
        raise ValueError("REFUSED:REQUIREMENT_DIGEST_DRIFT")
    if admission.get("capability") != requirement.get("capability"):
        raise ValueError("REFUSED:CAPABILITY_DRIFT")
    require_git_sha(admission.get("lab_revision"), "REFUSED:LAB_REVISION_INVALID")


def manufacture(requirement, admission, ggen_revision):
    """Legacy Gall slice retained for compatibility; receipt v2 is fully replay-verified."""
    validate_inputs(requirement, admission)
    require_git_sha(ggen_revision, "REFUSED:GGEN_REVISION_INVALID")
    spec = requirement.get("manufacture_spec")
    if not isinstance(spec, dict):
        raise ValueError("REFUSED:MANUFACTURE_SPEC_MISSING")
    kind = spec.get("kind")
    if kind == "filesystem_write":
        path = require_portable_path(spec.get("path"))
        content = spec.get("content")
        if not isinstance(content, str):
            raise ValueError("REFUSED:PROGRAM_CONTENT_INVALID")
        program = {"kind": "filesystem_write", "path": path, "content": content}
        verifier = {"kind": "file_sha256", "path": path, "digest": hashlib.sha256(content.encode()).hexdigest()}
    elif kind == "noop":
        program = {"kind": "noop"}
        verifier = {"kind": "noop"}
    else:
        raise ValueError("REFUSED:PROGRAM_KIND_UNSUPPORTED")
    match_all = spec.get("match_all", {})
    if not isinstance(match_all, dict) or not all(isinstance(k, str) and isinstance(v, str) for k, v in match_all.items()):
        raise ValueError("REFUSED:MATCH_SPEC_INVALID")
    payload = {
        "schema": PAYLOAD_SCHEMA,
        "capability": requirement["capability"],
        "match_all": dict(sorted(match_all.items())),
        "consequence": requirement["consequence"],
        "program": program,
        "verifier": verifier,
    }
    receipt = {
        "schema": RECEIPT_SCHEMA,
        "standing": "ALIVE",
        "authority_class": "CONSTRUCT",
        "do_authority": False,
        "validator": VALIDATOR,
        "request_id": requirement["requirement_id"],
        "request_digest": sha256_json(requirement),
        "manifest_digest": sha256_json(payload),
        "artifact_set_digest": sha256_json([{"path": "compiled-capability.json", "sha256": hashlib.sha256(canonical_bytes(payload)).hexdigest()}]),
        "lab_revision": admission["lab_revision"],
        "ggen_revision": ggen_revision,
        "courts": list(REQUIRED_COURTS),
    }
    receipt["receipt_digest"] = receipt_digest(receipt)
    verify(payload, receipt, expected_ggen_revision=ggen_revision, expected_lab_revision=admission["lab_revision"], expected_requirement_digest=sha256_json(requirement))
    return payload, receipt


def verify(payload, receipt, *, expected_ggen_revision=None, expected_lab_revision=None, expected_requirement_digest=None):
    if not isinstance(payload, dict) or payload.get("schema") != PAYLOAD_SCHEMA:
        raise ValueError("REFUSED:PAYLOAD_SCHEMA_INVALID")
    if receipt.get("schema") != RECEIPT_SCHEMA or receipt.get("standing") != "ALIVE":
        raise ValueError("REFUSED:RECEIPT_INVALID")
    if receipt.get("authority_class") != "CONSTRUCT" or receipt.get("do_authority") is not False:
        raise ValueError("REFUSED:RECEIPT_AUTHORITY_ESCALATION")
    if receipt.get("validator") != VALIDATOR:
        raise ValueError("REFUSED:VALIDATOR_IDENTITY_DRIFT")
    require_git_sha(receipt.get("ggen_revision"), "REFUSED:GGEN_REVISION_INVALID")
    require_git_sha(receipt.get("lab_revision"), "REFUSED:LAB_REVISION_INVALID")
    if expected_ggen_revision is not None and receipt.get("ggen_revision") != expected_ggen_revision:
        raise ValueError("REFUSED:GGEN_REVISION_DRIFT")
    if expected_lab_revision is not None and receipt.get("lab_revision") != expected_lab_revision:
        raise ValueError("REFUSED:LAB_REVISION_DRIFT")
    if expected_requirement_digest is not None and receipt.get("request_digest") != expected_requirement_digest:
        raise ValueError("REFUSED:REQUIREMENT_DIGEST_DRIFT")
    if receipt.get("receipt_digest") != receipt_digest(receipt):
        raise ValueError("REFUSED:RECEIPT_DIGEST_MISMATCH")
    courts = receipt.get("courts")
    if not isinstance(courts, list) or tuple(sorted(courts)) != tuple(sorted(REQUIRED_COURTS)):
        raise ValueError("REFUSED:MANUFACTURE_COURTS_INCOMPLETE")
    if receipt.get("manifest_digest") != sha256_json(payload):
        raise ValueError("REFUSED:BUNDLE_DIGEST_MISMATCH")
    program = payload.get("program", {})
    verifier = payload.get("verifier", {})
    if program.get("kind") == "filesystem_write":
        if verifier.get("kind") != "file_sha256" or program.get("path") != verifier.get("path"):
            raise ValueError("REFUSED:PROGRAM_VERIFIER_NOT_CLOSED")
        if verifier.get("digest") != hashlib.sha256(program.get("content", "").encode()).hexdigest():
            raise ValueError("REFUSED:PROGRAM_VERIFIER_NOT_CLOSED")
    elif program.get("kind") == "noop":
        if verifier != {"kind": "noop"}:
            raise ValueError("REFUSED:PROGRAM_VERIFIER_NOT_CLOSED")
    else:
        raise ValueError("REFUSED:PROGRAM_KIND_UNSUPPORTED")
    return True


def main():
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="command", required=True)

    legacy = sub.add_parser("manufacture")
    legacy.add_argument("requirement")
    legacy.add_argument("admission")
    legacy.add_argument("--ggen-revision", required=True)
    legacy.add_argument("--bundle-out", required=True)
    legacy.add_argument("--receipt-out", required=True)

    seal = sub.add_parser("seal-manifest")
    seal.add_argument("request")
    seal.add_argument("manifest")
    seal.add_argument("--ggen-revision", required=True)
    seal.add_argument("--lab-revision", required=True)
    seal.add_argument("--receipt-out", required=True)

    args = parser.parse_args()
    try:
        if args.command == "manufacture":
            requirement = json.loads(Path(args.requirement).read_text())
            admission = json.loads(Path(args.admission).read_text())
            payload, receipt = manufacture(requirement, admission, args.ggen_revision)
            Path(args.bundle_out).write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")
        else:
            request = json.loads(Path(args.request).read_text())
            manifest = json.loads(Path(args.manifest).read_text())
            receipt = seal_manifest(request, manifest, ggen_revision=args.ggen_revision, lab_revision=args.lab_revision)
        Path(args.receipt_out).write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n")
    except (KeyError, TypeError, ValueError, json.JSONDecodeError) as exc:
        print(json.dumps({"standing": str(exc)}))
        return 2
    print(json.dumps({"standing": "ALIVE", "receipt_digest": receipt["receipt_digest"]}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
