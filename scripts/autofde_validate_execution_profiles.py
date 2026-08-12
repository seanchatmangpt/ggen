#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
from pathlib import Path

SCHEMA = "urn:autofde:execution-profile:v1"
GENERATOR = "ggen:autofde-execution-profile-pack"
FORBIDDEN = {
    "principal",
    "delegated_principal",
    "delegatedPrincipal",
    "nonce",
    "expires_at",
    "expiresAt",
    "execution_grant",
    "executionGrant",
    "permission_token",
    "permissionToken",
}
JSON_OBJECT_FIELDS = ("config_json", "payload_json", "expected_json", "input_schema_json")


class ProfileRefusal(ValueError):
    pass


def refuse(code: str) -> None:
    raise ProfileRefusal(f"REFUSED:{code}")


def parse_object(profile: dict, field: str) -> dict:
    value = profile.get(field)
    if not isinstance(value, str):
        refuse(f"{field.upper()}_LEXICAL_MISSING")
    try:
        parsed = json.loads(value)
    except json.JSONDecodeError:
        refuse(f"{field.upper()}_JSON_INVALID")
    if not isinstance(parsed, dict):
        refuse(f"{field.upper()}_MUST_BE_OBJECT")
    return parsed


def validate_profile(profile: dict) -> None:
    if not isinstance(profile, dict):
        refuse("PROFILE_NOT_OBJECT")
    present_forbidden = sorted(FORBIDDEN.intersection(profile))
    if present_forbidden:
        refuse("AUTHORITY_TOKEN_FIELD:" + ",".join(present_forbidden))
    if not isinstance(profile.get("profile_id"), str) or not profile["profile_id"].strip():
        refuse("PROFILE_ID_MISSING")
    if not isinstance(profile.get("provider"), str) or not profile["provider"].strip():
        refuse("PROVIDER_MISSING")
    revision = profile.get("benchmark_revision")
    if not isinstance(revision, str) or len(revision) != 40 or any(ch not in "0123456789abcdef" for ch in revision):
        refuse("BENCHMARK_REVISION_INVALID")
    if not isinstance(profile.get("source_ref"), str) or not profile["source_ref"].strip():
        refuse("SOURCE_REF_MISSING")
    if not isinstance(profile.get("derived_from"), str) or not profile["derived_from"].strip():
        refuse("DERIVED_FROM_MISSING")
    ref = profile.get("capability_ref")
    binding = profile.get("capability_binding")
    has_ref = isinstance(ref, str) and bool(ref.strip())
    has_binding = isinstance(binding, str) and bool(binding.strip())
    if has_ref == has_binding:
        refuse("CAPABILITY_SELECTOR_NOT_EXACTLY_ONE")
    parsed = {field: parse_object(profile, field) for field in JSON_OBJECT_FIELDS}
    if not parsed["expected_json"]:
        refuse("EXPECTED_JSON_VACUOUS")
    if not isinstance(parsed["input_schema_json"].get("type"), str):
        refuse("INPUT_SCHEMA_TYPE_MISSING")


def validate_document(value: dict) -> None:
    if not isinstance(value, dict):
        refuse("DOCUMENT_NOT_OBJECT")
    if value.get("schema") != SCHEMA:
        refuse("DOCUMENT_SCHEMA_INVALID")
    if value.get("generated_by") != GENERATOR:
        refuse("GENERATOR_IDENTITY_INVALID")
    if value.get("authority_mode") != "external-only":
        refuse("AUTHORITY_MODE_INVALID")
    profiles = value.get("profiles")
    if not isinstance(profiles, list) or not profiles:
        refuse("PROFILES_EMPTY")
    seen = set()
    for profile in profiles:
        validate_profile(profile)
        if profile["profile_id"] in seen:
            refuse("DUPLICATE_PROFILE_ID")
        seen.add(profile["profile_id"])


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("path")
    args = parser.parse_args()
    try:
        value = json.loads(Path(args.path).read_text())
        validate_document(value)
    except (OSError, json.JSONDecodeError, ProfileRefusal) as exc:
        print(json.dumps({"standing": str(exc)}))
        return 2
    print(json.dumps({"standing": "ALIVE", "profiles": len(value["profiles"])}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
