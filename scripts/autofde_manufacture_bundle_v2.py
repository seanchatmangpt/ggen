#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

REQ_SCHEMA = "autofde.engineering-requirement/1"
ADM_SCHEMA = "autofde.lab-admission/1"
PAYLOAD_SCHEMA = "autofde.compiled-capability/1"
RECEIPT_SCHEMA = "autofde.manufacture-receipt/2"
VALIDATOR = "ggen:autofde-capability-bundle/2"
REQUIRED_COURTS = (
    "authority_non_escalation",
    "canonical_sha256",
    "program_verifier_closure",
    "provenance_binding",
    "receipt_self_integrity",
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


def receipt_digest(receipt):
    unsigned = dict(receipt)
    unsigned.pop("receipt_digest", None)
    return sha256_json(unsigned)


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
    for key, refusal in (("admission_digest", "REFUSED:ADMISSION_DIGEST_INVALID"), ("powl_digest", "REFUSED:POWL_DIGEST_INVALID")):
        value = admission.get(key)
        if not isinstance(value, str) or not value.startswith("sha256:") or len(value) != 71:
            raise ValueError(refusal)


def manufacture(requirement, admission, ggen_revision):
    validate_inputs(requirement, admission)
    require_git_sha(ggen_revision, "REFUSED:GGEN_REVISION_INVALID")
    spec = requirement.get("manufacture_spec")
    if not isinstance(spec, dict):
        raise ValueError("REFUSED:MANUFACTURE_SPEC_MISSING")
    kind = spec.get("kind")
    if kind == "filesystem_write":
        path = spec.get("path")
        content = spec.get("content")
        if not isinstance(path, str) or not path or path.startswith("/") or ".." in Path(path).parts or "\\" in path:
            raise ValueError("REFUSED:PROGRAM_PATH_INVALID")
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
        "requirement_id": requirement["requirement_id"],
        "requirement_digest": sha256_json(requirement),
        "admission_digest": admission["admission_digest"],
        "powl_digest": admission["powl_digest"],
        "lab_revision": admission["lab_revision"],
        "ggen_revision": ggen_revision,
        "bundle_digest": raw_sha256(payload),
        "payload_schema": PAYLOAD_SCHEMA,
        "validator": VALIDATOR,
        "courts": list(REQUIRED_COURTS),
    }
    receipt["receipt_digest"] = receipt_digest(receipt)
    return payload, receipt


def verify(payload, receipt, *, expected_ggen_revision=None, expected_lab_revision=None, expected_requirement_digest=None):
    if not isinstance(payload, dict) or payload.get("schema") != PAYLOAD_SCHEMA:
        raise ValueError("REFUSED:PAYLOAD_SCHEMA_INVALID")
    if not isinstance(receipt, dict) or receipt.get("schema") != RECEIPT_SCHEMA:
        raise ValueError("REFUSED:RECEIPT_SCHEMA_INVALID")
    if receipt.get("standing") != "ALIVE":
        raise ValueError("REFUSED:MANUFACTURER_NOT_ALIVE")
    if receipt.get("authority_class") != "CONSTRUCT" or receipt.get("do_authority") is not False:
        raise ValueError("REFUSED:RECEIPT_AUTHORITY_ESCALATION")
    if receipt.get("validator") != VALIDATOR or receipt.get("payload_schema") != PAYLOAD_SCHEMA:
        raise ValueError("REFUSED:VALIDATOR_IDENTITY_DRIFT")
    require_git_sha(receipt.get("ggen_revision"), "REFUSED:GGEN_REVISION_INVALID")
    require_git_sha(receipt.get("lab_revision"), "REFUSED:LAB_REVISION_INVALID")
    if expected_ggen_revision is not None and receipt.get("ggen_revision") != expected_ggen_revision:
        raise ValueError("REFUSED:GGEN_REVISION_DRIFT")
    if expected_lab_revision is not None and receipt.get("lab_revision") != expected_lab_revision:
        raise ValueError("REFUSED:LAB_REVISION_DRIFT")
    if expected_requirement_digest is not None and receipt.get("requirement_digest") != expected_requirement_digest:
        raise ValueError("REFUSED:REQUIREMENT_DIGEST_DRIFT")
    if receipt.get("receipt_digest") != receipt_digest(receipt):
        raise ValueError("REFUSED:RECEIPT_DIGEST_MISMATCH")
    courts = receipt.get("courts")
    if not isinstance(courts, list) or tuple(sorted(courts)) != tuple(sorted(REQUIRED_COURTS)):
        raise ValueError("REFUSED:MANUFACTURE_COURTS_INCOMPLETE")
    if raw_sha256(payload) != receipt.get("bundle_digest"):
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
    parser.add_argument("requirement")
    parser.add_argument("admission")
    parser.add_argument("--ggen-revision", required=True)
    parser.add_argument("--bundle-out", required=True)
    parser.add_argument("--receipt-out", required=True)
    args = parser.parse_args()
    try:
        requirement = json.loads(Path(args.requirement).read_text())
        admission = json.loads(Path(args.admission).read_text())
        payload, receipt = manufacture(requirement, admission, args.ggen_revision)
        verify(payload, receipt, expected_ggen_revision=args.ggen_revision, expected_lab_revision=admission["lab_revision"], expected_requirement_digest=sha256_json(requirement))
    except (KeyError, TypeError, ValueError, json.JSONDecodeError) as exc:
        print(json.dumps({"standing": str(exc)}))
        return 2
    Path(args.bundle_out).write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")
    Path(args.receipt_out).write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n")
    print(json.dumps({"standing": "ALIVE", "bundle_digest": receipt["bundle_digest"], "receipt_digest": receipt["receipt_digest"]}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
