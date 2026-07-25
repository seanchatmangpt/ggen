#!/usr/bin/env python3
"""Print compact diagnostics for the first independent transcription mismatch."""

from pathlib import Path
import validate_transcription_360 as validator


def first_difference(expected: bytes, actual: bytes) -> int:
    for index, (left, right) in enumerate(zip(expected, actual)):
        if left != right:
            return index
    return min(len(expected), len(actual))


records = validator.parse_records(validator.read_raw(validator.ONTOLOGY))
targets = {
    "admission/081-syntax-not-admission.md",
    "listings/081-81-syntax-is-not-admission.ttl",
}
for record in records:
    if record.path not in targets:
        continue
    expected = record.source.rstrip().encode("utf-8")
    actual = (validator.SRC / Path(record.path)).read_bytes()
    index = first_difference(expected, actual)
    start = max(0, index - 80)
    end = index + 160
    print(f"DIFF {record.path}")
    print(f"subject={record.subject} expected_len={len(expected)} actual_len={len(actual)} first_diff={index}")
    print(f"expected_slice={expected[start:end]!r}")
    print(f"actual_slice={actual[start:end]!r}")
    print(f"expected_crlf={expected.count(bytes([13,10]))} actual_crlf={actual.count(bytes([13,10]))}")
    print(f"expected_lf={expected.count(bytes([10]))} actual_lf={actual.count(bytes([10]))}")
