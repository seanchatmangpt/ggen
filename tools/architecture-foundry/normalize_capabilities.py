#!/usr/bin/env python3
"""Apply the exact rustfmt-equivalent structural rewrites for Workstream C bootstrap."""

from pathlib import Path

PATH = Path(__file__).parent / "src" / "bin" / "admit_capabilities.rs"


def replace(text: str, old: str, new: str) -> str:
    if old in text:
        return text.replace(old, new, 1)
    if new not in text:
        raise SystemExit(f"normalization anchor missing: {old!r}")
    return text


def main() -> None:
    text = PATH.read_text()
    replacements = [
        (
            "        if capability.capability_id.is_empty() || !seen_ids.insert(capability.capability_id.clone()) {",
            "        if capability.capability_id.is_empty()\n            || !seen_ids.insert(capability.capability_id.clone())\n        {",
        ),
        (
            '        ("foundry/catalogs/capabilities.json", capability_bytes.as_slice()),\n        ("foundry/catalogs/provenance.json", provenance_bytes.as_slice()),',
            '        (\n            "foundry/catalogs/capabilities.json",\n            capability_bytes.as_slice(),\n        ),\n        (\n            "foundry/catalogs/provenance.json",\n            provenance_bytes.as_slice(),\n        ),',
        ),
        (
            "fn derive_owner(\n    disposition: &str,\n    replacement_owner: &str,\n    historical_owner: &str,\n    subsystem: &str,\n) -> Result<String> {",
            "fn derive_owner(\n    disposition: &str, replacement_owner: &str, historical_owner: &str, subsystem: &str,\n) -> Result<String> {",
        ),
        (
            '        "REPLACED" | "SUBSUMED" => bail!(\n            "REPLACEMENT_OWNER_MISSING: disposition={disposition}, subsystem={subsystem}"\n        ),',
            '        "REPLACED" | "SUBSUMED" => {\n            bail!("REPLACEMENT_OWNER_MISSING: disposition={disposition}, subsystem={subsystem}")\n        }',
        ),
        (
            "fn require_clean(\n    snapshot: &ggen_architecture_foundry::RepositorySnapshot,\n    code: &str,\n) -> Result<()> {",
            "fn require_clean(\n    snapshot: &ggen_architecture_foundry::RepositorySnapshot, code: &str,\n) -> Result<()> {",
        ),
    ]
    for old, new in replacements:
        text = replace(text, old, new)
    PATH.write_text(text)


if __name__ == "__main__":
    main()
