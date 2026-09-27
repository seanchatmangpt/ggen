from semantic_jira import CS2_SUBJECT, Consumer, digest, project

def test_projection_is_order_independent():
    a = Consumer("CS2-WRK-003", "ggen-marketplace", "pack")
    b = Consumer("CS2-WRK-012", "ash-a2a", "triage")
    assert digest(project(CS2_SUBJECT, "CS2-CHICAGO", [a,b])) == digest(project(CS2_SUBJECT, "CS2-CHICAGO", [b,a]))

def test_divergent_subject_refused():
    try:
        project("https://example.invalid/wrong", "CS2-CHICAGO", [])
    except ValueError as e:
        assert "divergent CS2 subject" in str(e)
    else:
        raise AssertionError("divergent subject was admitted")
