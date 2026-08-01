#!/usr/bin/env python3
"""Distinguish unresolved replacement-owner sentinels from concrete owners."""

from pathlib import Path

TARGET = Path("tools/architecture-foundry/src/bin/admit_capabilities.rs")


def replace_once(text: str, old: str, new: str, label: str) -> str:
    if new in text:
        return text
    count = text.count(old)
    if count != 1:
        raise SystemExit(f"PATCH_REFUSED[{label}]: expected one match, observed {count}")
    return text.replace(old, new, 1)


def main() -> None:
    text = TARGET.read_text(encoding="utf-8")

    text = replace_once(
        text,
        '''        if !capability.replacement_owner.trim().is_empty() {\n            bail!(\n                "ARCHIVE_POLICY_REPLACEMENT_OWNER_PRESENT: {}",\n                decision.capability_id\n            );\n        }''',
        '''        if is_confirmed_replacement_owner(&capability.replacement_owner) {\n            bail!(\n                "ARCHIVE_POLICY_REPLACEMENT_OWNER_PRESENT: {}:{}",\n                decision.capability_id,\n                capability.replacement_owner\n            );\n        }''',
        "archive-owner-policy",
    )

    marker = "fn verify_recovery_refs(source_repo: &Path, decision: &DispositionDecision) -> Result<()> {"
    if "fn is_confirmed_replacement_owner(" not in text:
        if text.count(marker) != 1:
            raise SystemExit("PATCH_REFUSED[owner-helper-anchor]")
        helper = r'''fn is_confirmed_replacement_owner(value: &str) -> bool {
    let normalized = value.trim();
    if normalized.is_empty() {
        return false;
    }
    let upper = normalized.to_ascii_uppercase();
    !(upper == "UNKNOWN"
        || upper.starts_with("UNKNOWN ")
        || upper.starts_with("UNKNOWN-")
        || upper == "UNASSIGNED")
}

'''
        text = text.replace(marker, helper + marker, 1)

    TARGET.write_text(text, encoding="utf-8")


if __name__ == "__main__":
    main()
