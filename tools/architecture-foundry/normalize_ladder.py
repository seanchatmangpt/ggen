#!/usr/bin/env python3
"""Apply bounded ownership repairs before canonical formatting of the A-K ladder."""

from pathlib import Path

PATH = Path(__file__).parent / "src" / "bin" / "admit_final.rs"


def normalize(text: str, old: str, new: str) -> str:
    if old in text:
        return text.replace(old, new, 1)
    if new not in text:
        raise SystemExit(f"normalization anchor missing: {old!r}")
    return text


def main() -> None:
    text = PATH.read_text()
    replacements = [
        ('"program_id": program.program_id,', '"program_id": program.program_id.clone(),'),
        (
            '"program_digest": validation.program_digest,',
            '"program_digest": validation.program_digest.clone(),',
        ),
        ('"source_head": source.head,', '"source_head": source.head.clone(),'),
        ('"corpus_parent_head": corpus.head,', '"corpus_parent_head": corpus.head.clone(),'),
        (
            'inputs.insert("work-program".to_string(), validation.program_digest);',
            'inputs.insert("work-program".to_string(), validation.program_digest.clone());',
        ),
        (
            'inputs.insert("source-tree".to_string(), source.tracked_tree_digest);',
            'inputs.insert("source-tree".to_string(), source.tracked_tree_digest.clone());',
        ),
        (
            'inputs.insert("corpus-tree".to_string(), corpus.tracked_tree_digest);',
            'inputs.insert("corpus-tree".to_string(), corpus.tracked_tree_digest.clone());',
        ),
        ('subject: program.program_name,', 'subject: program.program_name.clone(),'),
        ('source_head: source.head,', 'source_head: source.head.clone(),'),
        ('corpus_head: corpus.head,', 'corpus_head: corpus.head.clone(),'),
    ]
    for old, new in replacements:
        text = normalize(text, old, new)
    PATH.write_text(text)


if __name__ == "__main__":
    main()
