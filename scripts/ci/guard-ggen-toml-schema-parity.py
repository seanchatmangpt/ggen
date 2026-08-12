#!/usr/bin/env python3
"""guard-ggen-toml-schema-parity.py — catches real field-TYPE drift between
ggen.toml's two independently-defined, incompatible schemas
(`.claude/rules/architecture.md`'s "ggen.toml has two schemas" section):

  - ggen_config::manifest::types (the "declarative-rules" schema, chosen
    when [[generation.rules]] is present)
  - ggen_engine::config (the "frontmatter" schema, chosen otherwise)

Both parse the SAME table names ([project], [ontology], [templates], [law])
under genuinely, deliberately DIFFERENT shapes -- e.g. ggen-config's
ProjectConfig carries version/description/authors/license that
ggen-engine's Project (name-only) does not. A "field sets must match" guard
would be wrong: it would fail on real, intentional differences that are not
bugs. What IS a real risk, and what this guard actually checks: a field
NAME that exists in BOTH schemas under the SAME table meaning a DIFFERENT
Rust base type on each side -- the concrete failure mode of someone editing
one schema's field, assuming (wrongly) it means the same thing in the
other. Field names present in only one schema are not flagged; a base-type
mismatch on a shared name is.

RATCHET, not a from-scratch hard-fail, matching guard-pack-e2e-coverage.sh's
convention: this pass found ZERO type mismatches among the fields the two
schemas actually share by name (rules: Vec<PathBuf> in both [law] structs;
name: String in both [project] structs; source: PathBuf in both [ontology]
structs; prefixes: BTreeMap<String,String> in both [ontology] structs) --
baseline is 0. A future edit that introduces a real mismatch fails this
guard; the baseline only ever moves down (a mismatch was fixed) via a
conscious edit to BASELINE_MISMATCH_COUNT below, never silently up.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

# (table name, struct name, source file, tag prefix used only in output)
STRUCTS = [
    ("project", "ProjectConfig", REPO_ROOT / "crates/ggen-config/src/manifest/types.rs"),
    ("project", "Project", REPO_ROOT / "crates/ggen-engine/src/config.rs"),
    ("ontology", "OntologyConfig", REPO_ROOT / "crates/ggen-config/src/manifest/types.rs"),
    ("ontology", "Ontology", REPO_ROOT / "crates/ggen-engine/src/config.rs"),
    ("templates", "TemplatesConfig", REPO_ROOT / "crates/ggen-config/src/config_lib/schema.rs"),
    ("templates", "Templates", REPO_ROOT / "crates/ggen-engine/src/config.rs"),
    ("law", "Law", REPO_ROOT / "crates/ggen-config/src/manifest/types.rs"),
    ("law", "Law", REPO_ROOT / "crates/ggen-engine/src/config.rs"),
]

# Real, checked-in-history baseline: 0 real type mismatches found among
# shared field names as of 2026-08-12. Only lower this via a conscious edit
# when a real mismatch is fixed; a run reporting MORE than this is a hard
# failure (a new, real mismatch was introduced).
BASELINE_MISMATCH_COUNT = 0

FIELD_RE = re.compile(
    r"pub\s+(?P<name>[a-z_][a-z0-9_]*)\s*:\s*(?P<ty>[A-Za-z0-9_:<>,\s]+?)\s*,",
    re.MULTILINE,
)


def extract_struct_body(path: Path, struct_name: str) -> str:
    text = path.read_text()
    # Match `pub struct <name> {` (possibly `pub struct <name>` on its own
    # line followed by `{`), capture up to the matching closing brace at
    # column 0 -- good enough for this repo's consistently-rustfmt'd style,
    # not a general Rust parser.
    pattern = re.compile(
        rf"pub struct {re.escape(struct_name)}\s*\{{(.*?)\n\}}", re.DOTALL
    )
    m = pattern.search(text)
    if not m:
        raise SystemExit(
            f"guard-ggen-toml-schema-parity: could not find `pub struct {struct_name}` "
            f"in {path} -- has it been renamed/moved? Update STRUCTS above."
        )
    return m.group(1)


def extract_fields(body: str) -> dict[str, str]:
    fields: dict[str, str] = {}
    for m in FIELD_RE.finditer(body):
        fields[m.group("name")] = re.sub(r"\s+", " ", m.group("ty").strip())
    return fields


def base_type(ty: str) -> str:
    """Strip one layer of Option<...>/Vec<...> wrapping so `Option<String>`
    and `String` aren't flagged as a mismatch purely over optionality --
    the real risk this guard targets is a genuinely different underlying
    type (e.g. String vs PathBuf, or a BTreeMap vs a Vec), not
    required-vs-optional, which `#[serde(default)]` already governs."""
    m = re.fullmatch(r"(?:Option|Vec)<(.+)>", ty)
    return m.group(1).strip() if m else ty


def main() -> int:
    by_table: dict[str, list[tuple[str, dict[str, str]]]] = {}
    for table, struct_name, path in STRUCTS:
        body = extract_struct_body(path, struct_name)
        fields = extract_fields(body)
        by_table.setdefault(table, []).append((f"{struct_name} ({path.name})", fields))

    mismatches: list[str] = []
    for table, entries in by_table.items():
        if len(entries) != 2:
            continue
        (label_a, fields_a), (label_b, fields_b) = entries
        shared_names = sorted(set(fields_a) & set(fields_b))
        for name in shared_names:
            ty_a, ty_b = base_type(fields_a[name]), base_type(fields_b[name])
            if ty_a != ty_b:
                mismatches.append(
                    f"[{table}].{name}: {label_a} has `{fields_a[name]}`, "
                    f"{label_b} has `{fields_b[name]}` (base types `{ty_a}` != `{ty_b}`)"
                )

    if mismatches:
        print(
            f"guard-ggen-toml-schema-parity: {len(mismatches)} real field-name/type "
            f"mismatch(es) between ggen-config and ggen-engine's ggen.toml schemas:",
            file=sys.stderr,
        )
        for line in mismatches:
            print(f"  - {line}", file=sys.stderr)

    if len(mismatches) > BASELINE_MISMATCH_COUNT:
        print(
            f"FAIL: {len(mismatches)} mismatches exceeds baseline "
            f"{BASELINE_MISMATCH_COUNT} -- a real cross-schema drift was introduced.",
            file=sys.stderr,
        )
        return 1

    print(
        f"OK: guard-ggen-toml-schema-parity: {len(mismatches)} mismatches "
        f"(baseline {BASELINE_MISMATCH_COUNT})."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
