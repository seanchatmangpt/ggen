"""Materialize deterministic CS2 Jira payloads from normalized rows."""
from semantic_jira import CS2_SUBJECT, Consumer, canonical_json, jira_payload, project

def materialize(rows):
    consumers = [
        Consumer(
            work_id=row["workId"],
            iri=row["consumer"],
            expected_projection=row["expectedProjection"],
        )
        for row in rows
    ]
    issues = project(CS2_SUBJECT, "CS2-CHICAGO", consumers)
    return {
        "subject": CS2_SUBJECT,
        "count": len(issues),
        "canonical": canonical_json(issues),
        "payloads": [jira_payload(issue) for issue in issues],
    }
