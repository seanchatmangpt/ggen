#!/usr/bin/env python3
from __future__ import annotations
import argparse, hashlib, json
from pathlib import Path

REQ_SCHEMA = "autofde.engineering-requirement/1"
ADM_SCHEMA = "autofde.lab-admission/1"
PAYLOAD_SCHEMA = "autofde.compiled-capability/1"
RECEIPT_SCHEMA = "autofde.manufacture-receipt/1"


def canonical_bytes(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()


def sha256_json(value):
    return "sha256:" + hashlib.sha256(canonical_bytes(value)).hexdigest()


def raw_sha256(value):
    return hashlib.sha256(canonical_bytes(value)).hexdigest()


def validate_inputs(requirement, admission):
    if requirement.get("schema") != REQ_SCHEMA or requirement.get("standing") != "BLOCKED:CAPABILITY_ABSENT":
        raise ValueError("REFUSED:REQUIREMENT_NOT_ADMITTED")
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


def manufacture(requirement, admission, ggen_revision):
    validate_inputs(requirement, admission)
    if not ggen_revision:
        raise ValueError("REFUSED:GGEN_REVISION_MISSING")
    spec = requirement.get("manufacture_spec")
    if not isinstance(spec, dict):
        raise ValueError("REFUSED:MANUFACTURE_SPEC_MISSING")
    kind = spec.get("kind")
    if kind == "filesystem_write":
        path = spec.get("path")
        content = spec.get("content")
        if not isinstance(path, str) or not path or path.startswith("/") or ".." in Path(path).parts:
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
    bundle_digest = raw_sha256(payload)
    receipt = {
        "schema": RECEIPT_SCHEMA,
        "standing": "ALIVE",
        "authority_class": "CONSTRUCT",
        "do_authority": False,
        "requirement_id": requirement["requirement_id"],
        "admission_digest": admission["admission_digest"],
        "powl_digest": admission["powl_digest"],
        "lab_revision": admission["lab_revision"],
        "ggen_revision": ggen_revision,
        "bundle_digest": bundle_digest,
        "payload_schema": PAYLOAD_SCHEMA,
        "validator": "ggen:autofde-capability-bundle/1",
        "tests": ["canonical_sha256", "program_verifier_closure", "authority_non_escalation"],
    }
    receipt["receipt_digest"] = sha256_json(receipt)
    return payload, receipt


def verify(payload, receipt):
    if raw_sha256(payload) != receipt.get("bundle_digest"):
        raise ValueError("REFUSED:BUNDLE_DIGEST_MISMATCH")
    program = payload.get("program", {})
    verifier = payload.get("verifier", {})
    if program.get("kind") == "filesystem_write":
        if verifier.get("kind") != "file_sha256" or program.get("path") != verifier.get("path"):
            raise ValueError("REFUSED:PROGRAM_VERIFIER_NOT_CLOSED")
        if verifier.get("digest") != hashlib.sha256(program.get("content", "").encode()).hexdigest():
            raise ValueError("REFUSED:PROGRAM_VERIFIER_NOT_CLOSED")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("requirement")
    parser.add_argument("admission")
    parser.add_argument("--ggen-revision", required=True)
    parser.add_argument("--bundle-out", required=True)
    parser.add_argument("--receipt-out", required=True)
    args = parser.parse_args()
    try:
        payload, receipt = manufacture(
            json.loads(Path(args.requirement).read_text()),
            json.loads(Path(args.admission).read_text()),
            args.ggen_revision,
        )
        verify(payload, receipt)
    except ValueError as exc:
        print(json.dumps({"standing": str(exc)}))
        return 2
    Path(args.bundle_out).write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")
    Path(args.receipt_out).write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n")
    print(json.dumps({"standing": "ALIVE", "bundle_digest": receipt["bundle_digest"], "receipt_digest": receipt["receipt_digest"]}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
