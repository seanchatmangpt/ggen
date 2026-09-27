import json
from pathlib import Path
import pytest
from adapters.semantic_jira import (
    AUTHORITY, EXACT_SUBJECT, batches, load_projection,
    project_jira_payloads, projection_digest, semantic_diff,
)

def fixture():
    return {
        "schema": "https://chatman.ai/cs2/semantic-jira/v1",
        "exact_subject": EXACT_SUBJECT,
        "authority_ceiling": AUTHORITY,
        "issues": [{
            "external_id": "CS2-WRK-014",
            "summary": "CS2 projection for ggen-igniter",
            "description": "semantic Jira consumer",
            "labels": ["cs2"],
            "properties": {
                "subject": EXACT_SUBJECT,
                "consumer": "https://chatman.ai/cs2#ggen-igniter",
                "authority_ceiling": AUTHORITY,
            },
        }],
    }

def test_projection_is_deterministic_and_construct_only():
    issues = load_projection(json.dumps(fixture()))
    payloads = project_jira_payloads(issues)
    assert payloads[0]["fields"]["properties"]["cs2.authority_ceiling"] == "CONSTRUCT"
    assert projection_digest(payloads) == projection_digest(project_jira_payloads(issues))

def test_divergent_subject_refused():
    data = fixture()
    data["exact_subject"] = "https://example.invalid/other"
    with pytest.raises(ValueError, match="divergent"):
        load_projection(json.dumps(data))

def test_semantic_diff_and_batches():
    before = load_projection(json.dumps(fixture()))
    after = ()
    assert semantic_diff(before, after)["removed"] == ["CS2-WRK-014"]
    assert batches(before, 1) == [before]
