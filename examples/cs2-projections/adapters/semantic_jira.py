"""Pure CS2 semantic Jira projection helpers; no remote actuation."""
import hashlib
import json

EXACT_SUBJECT = "https://chatman.ai/cs2#RFC-CS2-001"
AUTHORITY = "CONSTRUCT"

def load_projection(raw):
    value = json.loads(raw)
    if value.get("exact_subject") != EXACT_SUBJECT:
        raise ValueError("divergent exact subject")
    if value.get("authority_ceiling") != AUTHORITY:
        raise ValueError("authority ceiling is not CONSTRUCT")
    rows = value.get("issues", [])
    for row in rows:
        if row.get("subject", row.get("properties", {}).get("subject")) != EXACT_SUBJECT:
            raise ValueError("divergent issue subject")
        if row.get("authority_ceiling", row.get("properties", {}).get("authority_ceiling")) != AUTHORITY:
            raise ValueError("issue exceeds authority ceiling")
    return tuple(sorted(rows, key=lambda x: (x["external_id"], x.get("consumer", ""))))

def canonical_bytes(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":")).encode()

def projection_digest(value):
    return hashlib.sha256(canonical_bytes(value)).hexdigest()

def semantic_diff(before, after):
    a = {x["external_id"]: x for x in before}
    b = {x["external_id"]: x for x in after}
    return {
        "added": sorted(b.keys() - a.keys()),
        "removed": sorted(a.keys() - b.keys()),
        "changed": sorted(k for k in a.keys() & b.keys() if a[k] != b[k]),
    }

def batches(rows, size=50):
    if size < 1:
        raise ValueError("size must be positive")
    ordered = tuple(sorted(rows, key=lambda x: (x["external_id"], x.get("consumer", ""))))
    return [ordered[i:i + size] for i in range(0, len(ordered), size)]
