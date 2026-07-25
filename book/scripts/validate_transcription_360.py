#!/usr/bin/env python3
"""Independently validate the complete book transcription surface.

This validator does not call ggen and does not trust the generated product. It:

1. lexes the authoritative pack ontology outside Turtle string literals;
2. extracts every top-level book:Chapter and book:Listing sourcePath/sourceText;
3. validates the two Tera/SPARQL templates against their declared contracts;
4. reconstructs the expected rendered bytes (`source | trim_end`);
5. compares every expected byte with book/src;
6. refuses missing, extra, duplicate, escaping, traversal, or count drift;
7. optionally sabotages TTL, Tera, and generated output to prove sensitivity.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import argparse
import hashlib
import re
import shutil
import tempfile


REPO = Path(__file__).resolve().parents[2]
PACK = REPO / "packs" / "level-five-book-pack"
ONTOLOGY = PACK / "ontology.ttl"
TEMPLATES = PACK / "templates"
SRC = REPO / "book" / "src"

EXPECTED_CHAPTERS = 367
EXPECTED_LISTINGS = 367
MANUAL_OUTPUTS = {"README.md"}


class ValidationError(RuntimeError):
    pass


@dataclass(frozen=True)
class Record:
    kind: str
    subject: str
    path: str
    source: str
    source_content_start: int
    source_content_end: int


def fail(message: str) -> None:
    raise ValidationError(message)


def skip_quoted(text: str, start: int, delimiter: str) -> int:
    """Return the index immediately after a Turtle quoted literal."""
    if not text.startswith(delimiter, start):
        fail(f"literal does not start with {delimiter!r} at byte {start}")
    i = start + len(delimiter)
    while i < len(text):
        if text.startswith(delimiter, i):
            return i + len(delimiter)
        if text[i] == "\\":
            i += 2
        else:
            i += 1
    fail(f"unterminated Turtle literal beginning at byte {start}")
    raise AssertionError("unreachable")


def top_level_subjects(text: str) -> list[tuple[int, str, str]]:
    """Find chapter/listing subjects only while outside Turtle literals/comments.

    RDF type is authoritative. Subject identifiers are intentionally not used to
    infer type because historical listing individuals may retain chapter-prefixed
    names such as `book:chapter-...-listing`.
    """
    pattern = re.compile(
        r"book:(?P<subject>(?:chapter|listing)-[^\s]+)\s+"
        r"a\s+book:(?P<class_name>Chapter|Listing)\s*;"
    )
    found: list[tuple[int, str, str]] = []
    i = 0
    while i < len(text):
        if text.startswith('"""', i):
            i = skip_quoted(text, i, '"""')
            continue
        if text[i] == '"':
            i = skip_quoted(text, i, '"')
            continue
        if text[i] == "#":
            newline = text.find("\n", i)
            i = len(text) if newline < 0 else newline + 1
            continue
        if text[i] == "<":
            end = text.find(">", i + 1)
            if end < 0:
                fail(f"unterminated IRI beginning at byte {i}")
            i = end + 1
            continue
        if i == 0 or text[i - 1] == "\n":
            j = i
            while j < len(text) and text[j] in " \t":
                j += 1
            match = pattern.match(text, j)
            if match:
                found.append((j, match.group("class_name"), match.group("subject")))
        i += 1
    return found


def find_token_outside(text: str, token: str) -> int:
    i = 0
    while i < len(text):
        if text.startswith('"""', i):
            i = skip_quoted(text, i, '"""')
            continue
        if text[i] == '"':
            i = skip_quoted(text, i, '"')
            continue
        if text[i] == "#":
            newline = text.find("\n", i)
            i = len(text) if newline < 0 else newline + 1
            continue
        if text.startswith(token, i):
            return i
        i += 1
    return -1


def decode_turtle(raw: str) -> str:
    escapes = {
        "t": "\t",
        "b": "\b",
        "n": "\n",
        "r": "\r",
        "f": "\f",
        '"': '"',
        "'": "'",
        "\\": "\\",
    }
    output: list[str] = []
    i = 0
    while i < len(raw):
        if raw[i] != "\\":
            output.append(raw[i])
            i += 1
            continue
        if i + 1 >= len(raw):
            fail("dangling Turtle escape")
        code = raw[i + 1]
        if code in escapes:
            output.append(escapes[code])
            i += 2
            continue
        if code in {"u", "U"}:
            width = 4 if code == "u" else 8
            digits = raw[i + 2 : i + 2 + width]
            if len(digits) != width or not re.fullmatch(r"[0-9A-Fa-f]+", digits):
                fail(f"invalid Turtle Unicode escape: \\{code}{digits}")
            output.append(chr(int(digits, 16)))
            i += 2 + width
            continue
        fail(f"unsupported Turtle escape: \\{code}")
    return "".join(output)


def parse_literal(text: str, start: int, delimiter: str) -> tuple[str, int, int, int]:
    end_after = skip_quoted(text, start, delimiter)
    content_start = start + len(delimiter)
    content_end = end_after - len(delimiter)
    return decode_turtle(text[content_start:content_end]), end_after, content_start, content_end


def property_literal(segment: str, token: str, delimiter: str) -> tuple[str, int, int]:
    token_pos = find_token_outside(segment, token)
    if token_pos < 0:
        fail(f"record missing {token}")
    literal_pos = segment.find(delimiter, token_pos + len(token))
    if literal_pos < 0:
        fail(f"{token} has no {delimiter!r} literal")
    value, _, content_start, content_end = parse_literal(segment, literal_pos, delimiter)
    return value, content_start, content_end


def parse_records(ontology_text: str) -> list[Record]:
    subjects = top_level_subjects(ontology_text)
    records: list[Record] = []
    for index, (start, kind, subject) in enumerate(subjects):
        end = subjects[index + 1][0] if index + 1 < len(subjects) else len(ontology_text)
        segment = ontology_text[start:end]
        path, _, _ = property_literal(segment, "book:sourcePath", '"')
        source, source_start, source_end = property_literal(segment, "book:sourceText", '"""')
        records.append(
            Record(
                kind=kind,
                subject=f"book:{subject}",
                path=path,
                source=source,
                source_content_start=start + source_start,
                source_content_end=start + source_end,
            )
        )
    return records


def validate_template(path: Path, *, kind: str, variable: str) -> None:
    text = path.read_text(encoding="utf-8").replace("\r\n", "\n")
    required = [
        'to: "src/{{ path }}"',
        "SELECT ?path ?source WHERE {",
        f"?{variable} a book:{kind} ;",
        "book:sourcePath ?path ;",
        "book:sourceText ?source .",
        "} ORDER BY ?path",
    ]
    for marker in required:
        if text.count(marker) != 1:
            fail(f"{path}: expected exactly one template contract marker {marker!r}")
    if text.count("---") != 2:
        fail(f"{path}: expected one Tera front-matter block")
    body = text.split("---", 2)[2].strip()
    if body != "{{ source | trim_end }}":
        fail(f"{path}: output law must remain exactly '{{{{ source | trim_end }}}}'")


def digest(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def validate(
    *,
    ontology_path: Path = ONTOLOGY,
    templates_dir: Path = TEMPLATES,
    src_root: Path = SRC,
) -> list[Record]:
    template_files = {p.name for p in templates_dir.iterdir() if p.is_file()}
    expected_templates = {"chapter.md.tmpl", "listing.tmpl"}
    if template_files != expected_templates:
        fail(
            f"template inventory mismatch: expected={sorted(expected_templates)} "
            f"actual={sorted(template_files)}"
        )
    validate_template(
        templates_dir / "chapter.md.tmpl", kind="Chapter", variable="chapter"
    )
    validate_template(
        templates_dir / "listing.tmpl", kind="Listing", variable="listing"
    )

    ontology_text = ontology_path.read_text(encoding="utf-8")
    records = parse_records(ontology_text)
    chapters = [record for record in records if record.kind == "Chapter"]
    listings = [record for record in records if record.kind == "Listing"]
    if len(chapters) != EXPECTED_CHAPTERS or len(listings) != EXPECTED_LISTINGS:
        fail(
            "ontology census mismatch: "
            f"chapters={len(chapters)}/{EXPECTED_CHAPTERS} "
            f"listings={len(listings)}/{EXPECTED_LISTINGS}"
        )

    subjects = [record.subject for record in records]
    if len(subjects) != len(set(subjects)):
        fail("ontology contains duplicate chapter/listing subjects")

    paths = [record.path for record in records]
    if len(paths) != len(set(paths)):
        duplicates = sorted({path for path in paths if paths.count(path) > 1})
        fail(f"multiple ontology records own the same output path: {duplicates}")

    expected_paths: set[str] = set()
    mismatches: list[str] = []
    for record in records:
        relative = Path(record.path)
        if relative.is_absolute() or ".." in relative.parts:
            fail(f"{record.subject}: unsafe output path {record.path!r}")
        expected_paths.add(relative.as_posix())
        output_path = src_root / relative
        if not output_path.is_file():
            mismatches.append(f"missing {record.path} for {record.subject}")
            continue
        expected = record.source.rstrip()
        actual = output_path.read_text(encoding="utf-8")
        if actual != expected:
            mismatches.append(
                f"byte mismatch {record.path}: expected_sha256={digest(expected)} "
                f"actual_sha256={digest(actual)} subject={record.subject}"
            )

    actual_paths = {
        path.relative_to(src_root).as_posix()
        for path in src_root.rglob("*")
        if path.is_file()
    }
    missing = sorted(expected_paths - actual_paths)
    extra = sorted(actual_paths - expected_paths - MANUAL_OUTPUTS)
    if missing:
        mismatches.append(f"missing generated outputs: {missing}")
    if extra:
        mismatches.append(f"unowned generated outputs: {extra}")
    if mismatches:
        fail("TTL/Tera/output validation failed:\n" + "\n".join(mismatches[:25]))

    print(
        "OK 360: "
        f"ttl_chapters={len(chapters)} ttl_listings={len(listings)} "
        f"tera_templates={len(expected_templates)} generated_outputs={len(expected_paths)} "
        f"manual_outputs={len(MANUAL_OUTPUTS)} byte_mismatches=0 extras=0"
    )
    return records


def expect_refusal(label: str, operation) -> None:
    try:
        operation()
    except ValidationError as error:
        print(f"REFUSED sabotage {label}: {str(error).splitlines()[0]}")
        return
    fail(f"sabotage was not detected: {label}")


def sabotage() -> None:
    records = validate()
    chapter = next(record for record in records if record.kind == "Chapter")

    with tempfile.TemporaryDirectory(prefix="ggen-book-360-") as temp:
        root = Path(temp)
        temp_pack = root / "pack"
        temp_templates = temp_pack / "templates"
        temp_src = root / "src"
        temp_pack.mkdir()
        shutil.copy2(ONTOLOGY, temp_pack / "ontology.ttl")
        shutil.copytree(TEMPLATES, temp_templates)
        shutil.copytree(SRC, temp_src)

        output = temp_src / chapter.path
        original_output = output.read_text(encoding="utf-8")
        output.write_text(original_output + "\nSABOTAGE_OUTPUT", encoding="utf-8")
        expect_refusal(
            "generated-output-byte",
            lambda: validate(
                ontology_path=temp_pack / "ontology.ttl",
                templates_dir=temp_templates,
                src_root=temp_src,
            ),
        )
        output.write_text(original_output, encoding="utf-8")

        ontology_text = (temp_pack / "ontology.ttl").read_text(encoding="utf-8")
        mutated = (
            ontology_text[: chapter.source_content_start]
            + "SABOTAGE_TTL"
            + ontology_text[chapter.source_content_start :]
        )
        (temp_pack / "ontology.ttl").write_text(mutated, encoding="utf-8")
        expect_refusal(
            "ontology-sourceText",
            lambda: validate(
                ontology_path=temp_pack / "ontology.ttl",
                templates_dir=temp_templates,
                src_root=temp_src,
            ),
        )
        (temp_pack / "ontology.ttl").write_text(ontology_text, encoding="utf-8")

        chapter_template = temp_templates / "chapter.md.tmpl"
        original_template = chapter_template.read_text(encoding="utf-8")
        chapter_template.write_text(
            original_template.replace("source | trim_end", "source | trim"),
            encoding="utf-8",
        )
        expect_refusal(
            "tera-output-law",
            lambda: validate(
                ontology_path=temp_pack / "ontology.ttl",
                templates_dir=temp_templates,
                src_root=temp_src,
            ),
        )

    print("OK sabotage: TTL drift, Tera-law drift, and generated-byte drift all refused")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--sabotage",
        action="store_true",
        help="prove the validator detects TTL, Tera, and generated-output mutations",
    )
    args = parser.parse_args()
    if args.sabotage:
        sabotage()
    else:
        validate()


if __name__ == "__main__":
    try:
        main()
    except ValidationError as error:
        raise SystemExit(str(error)) from error
