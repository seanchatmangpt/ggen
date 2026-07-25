#!/usr/bin/env python3
"""Independent TTL → Tera contract → generated-byte validation for the book."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import argparse
import hashlib
import re
import shutil
import tempfile

REPO = Path(__file__).resolve().parents[2]
PACK = REPO / "packs/level-five-book-pack"
ONTOLOGY = PACK / "ontology.ttl"
TEMPLATES = PACK / "templates"
SRC = REPO / "book/src"
EXPECTED = {"Chapter": 367, "Listing": 367}
MANUAL = {"README.md", "SUMMARY.md", "theme/level-five.css"}
EXPRESSION = "{{ source | trim_end }}"


class ValidationError(RuntimeError):
    pass


@dataclass(frozen=True)
class Record:
    kind: str
    subject: str
    path: str
    source: str
    source_start: int


def fail(message: str) -> None:
    raise ValidationError(message)


def read_raw(path: Path) -> str:
    with path.open("r", encoding="utf-8", newline="") as stream:
        return stream.read()


def write_raw(path: Path, value: str) -> None:
    with path.open("w", encoding="utf-8", newline="") as stream:
        stream.write(value)


def skip_string(text: str, start: int, delimiter: str) -> int:
    if not text.startswith(delimiter, start):
        fail(f"expected {delimiter!r} at byte {start}")
    i = start + len(delimiter)
    while i < len(text):
        if text.startswith(delimiter, i):
            return i + len(delimiter)
        i += 2 if text[i] == "\\" else 1
    fail(f"unterminated Turtle literal at byte {start}")
    raise AssertionError("unreachable")


def top_subjects(text: str) -> list[tuple[int, str, str]]:
    pattern = re.compile(
        r"book:(?P<subject>(?:chapter|listing)-[^\s]+)\s+"
        r"a\s+book:(?P<kind>Chapter|Listing)\s*;"
    )
    found: list[tuple[int, str, str]] = []
    i = 0
    while i < len(text):
        if text.startswith('"""', i):
            i = skip_string(text, i, '"""')
            continue
        if text[i] == '"':
            i = skip_string(text, i, '"')
            continue
        if text[i] == "#":
            newline = text.find("\n", i)
            i = len(text) if newline < 0 else newline + 1
            continue
        if text[i] == "<":
            end = text.find(">", i + 1)
            if end < 0:
                fail(f"unterminated IRI at byte {i}")
            i = end + 1
            continue
        if i == 0 or text[i - 1] == "\n":
            j = i
            while j < len(text) and text[j] in " \t":
                j += 1
            match = pattern.match(text, j)
            if match:
                # RDF type is authoritative. Historical listing subjects may be
                # named book:chapter-...-listing.
                found.append((j, match.group("kind"), match.group("subject")))
        i += 1
    return found


def token_outside(text: str, token: str) -> int:
    i = 0
    while i < len(text):
        if text.startswith('"""', i):
            i = skip_string(text, i, '"""')
            continue
        if text[i] == '"':
            i = skip_string(text, i, '"')
            continue
        if text[i] == "#":
            newline = text.find("\n", i)
            i = len(text) if newline < 0 else newline + 1
            continue
        if text.startswith(token, i):
            return i
        i += 1
    return -1


def unescape_turtle(raw: str) -> str:
    simple = {
        "t": "\t", "b": "\b", "n": "\n", "r": "\r", "f": "\f",
        '"': '"', "'": "'", "\\": "\\",
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
        if code in simple:
            output.append(simple[code])
            i += 2
            continue
        if code in {"u", "U"}:
            width = 4 if code == "u" else 8
            digits = raw[i + 2 : i + 2 + width]
            if len(digits) != width or not re.fullmatch(r"[0-9A-Fa-f]+", digits):
                fail(f"invalid Turtle Unicode escape \\{code}{digits}")
            output.append(chr(int(digits, 16)))
            i += 2 + width
            continue
        fail(f"unsupported Turtle escape \\{code}")
    return "".join(output)


def literal(segment: str, token: str, delimiter: str) -> tuple[str, int]:
    token_pos = token_outside(segment, token)
    if token_pos < 0:
        fail(f"record missing {token}")
    start = segment.find(delimiter, token_pos + len(token))
    if start < 0:
        fail(f"{token} has no {delimiter!r} literal")
    end_after = skip_string(segment, start, delimiter)
    content_start = start + len(delimiter)
    content_end = end_after - len(delimiter)
    return unescape_turtle(segment[content_start:content_end]), content_start


def parse_records(ontology: str) -> list[Record]:
    subjects = top_subjects(ontology)
    records: list[Record] = []
    for index, (start, kind, subject) in enumerate(subjects):
        end = subjects[index + 1][0] if index + 1 < len(subjects) else len(ontology)
        segment = ontology[start:end]
        path, _ = literal(segment, "book:sourcePath", '"')
        source, source_start = literal(segment, "book:sourceText", '"""')
        records.append(Record(kind, f"book:{subject}", path, source, start + source_start))
    return records


def validate_template(path: Path, kind: str, variable: str) -> bytes:
    text = read_raw(path).replace("\r\n", "\n")
    markers = (
        'to: "src/{{ path }}"',
        "SELECT ?path ?source WHERE {",
        f"?{variable} a book:{kind} ;",
        "book:sourcePath ?path ;",
        "book:sourceText ?source .",
        "} ORDER BY ?path",
    )
    for marker in markers:
        if text.count(marker) != 1:
            fail(f"{path}: expected exactly one contract marker {marker!r}")
    if text.count("---") != 2:
        fail(f"{path}: invalid Tera front matter")
    body = text.split("---", 2)[2]
    if body.startswith("\n"):
        body = body[1:]
    if not body.startswith(EXPRESSION):
        fail(f"{path}: output law does not begin with {EXPRESSION}")
    suffix = body[len(EXPRESSION):]
    if suffix not in {"", "\n"}:
        fail(f"{path}: unsupported literal Tera suffix {suffix!r}")
    return suffix.encode("utf-8")


def sha(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def validate(
    ontology_path: Path = ONTOLOGY,
    templates: Path = TEMPLATES,
    src: Path = SRC,
) -> list[Record]:
    inventory = {path.name for path in templates.iterdir() if path.is_file()}
    if inventory != {"chapter.md.tmpl", "listing.tmpl"}:
        fail(f"template inventory mismatch: {sorted(inventory)}")
    suffixes = {
        "Chapter": validate_template(templates / "chapter.md.tmpl", "Chapter", "chapter"),
        "Listing": validate_template(templates / "listing.tmpl", "Listing", "listing"),
    }

    ontology = read_raw(ontology_path)
    records = parse_records(ontology)
    for kind, count in EXPECTED.items():
        observed = sum(record.kind == kind for record in records)
        if observed != count:
            fail(f"ontology census mismatch: {kind}={observed}/{count}")

    subjects = [record.subject for record in records]
    if len(subjects) != len(set(subjects)):
        fail("duplicate ontology subjects")
    paths = [record.path for record in records]
    if len(paths) != len(set(paths)):
        duplicates = sorted({path for path in paths if paths.count(path) > 1})
        fail(f"multiple records own one output: {duplicates}")

    expected_paths: set[str] = set()
    mismatches: list[str] = []
    for record in records:
        relative = Path(record.path)
        if relative.is_absolute() or ".." in relative.parts:
            fail(f"{record.subject}: unsafe path {record.path!r}")
        expected_paths.add(relative.as_posix())
        output = src / relative
        if not output.is_file():
            mismatches.append(f"missing {record.path} for {record.subject}")
            continue
        expected_bytes = record.source.rstrip().encode("utf-8") + suffixes[record.kind]
        actual_bytes = output.read_bytes()
        if actual_bytes != expected_bytes:
            mismatches.append(
                f"byte mismatch {record.path}: expected_sha256={sha(expected_bytes)} "
                f"actual_sha256={sha(actual_bytes)} subject={record.subject}"
            )

    actual_paths = {
        path.relative_to(src).as_posix()
        for path in src.rglob("*") if path.is_file()
    }
    missing = sorted(expected_paths - actual_paths)
    extra = sorted(actual_paths - expected_paths - MANUAL)
    if missing:
        mismatches.append(f"missing outputs: {missing}")
    if extra:
        mismatches.append(f"unowned outputs: {extra}")
    if mismatches:
        fail("TTL/Tera/output validation failed:\n" + "\n".join(mismatches[:25]))

    print(
        "OK 360: ttl_chapters=367 ttl_listings=367 tera_templates=2 "
        f"generated_outputs={len(expected_paths)} manual_outputs={len(MANUAL)} "
        "byte_mismatches=0 extras=0"
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
        original_output = output.read_bytes()
        output.write_bytes(original_output + b"\nSABOTAGE_OUTPUT")
        expect_refusal(
            "generated-output-byte",
            lambda: validate(temp_pack / "ontology.ttl", temp_templates, temp_src),
        )
        output.write_bytes(original_output)

        ontology = read_raw(temp_pack / "ontology.ttl")
        write_raw(
            temp_pack / "ontology.ttl",
            ontology[: chapter.source_start] + "SABOTAGE_TTL" + ontology[chapter.source_start :],
        )
        expect_refusal(
            "ontology-sourceText",
            lambda: validate(temp_pack / "ontology.ttl", temp_templates, temp_src),
        )
        write_raw(temp_pack / "ontology.ttl", ontology)

        template = temp_templates / "chapter.md.tmpl"
        original_template = read_raw(template)
        write_raw(template, original_template.replace("source | trim_end", "source | trim"))
        expect_refusal(
            "tera-output-law",
            lambda: validate(temp_pack / "ontology.ttl", temp_templates, temp_src),
        )
    print("OK sabotage: TTL, Tera, and generated-byte mutations all refused")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--sabotage", action="store_true")
    args = parser.parse_args()
    sabotage() if args.sabotage else validate()


if __name__ == "__main__":
    try:
        main()
    except ValidationError as error:
        raise SystemExit(str(error)) from error
