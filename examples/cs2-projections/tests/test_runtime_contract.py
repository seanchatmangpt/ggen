"""Contract fixtures authored but not executed in throughput mode."""
import pytest
from runtime import ConsumerProjection,Provenance,SubjectMismatch,AuthorityExceeded,materialize,replay_matches
P=Provenance("canonical.ttl","0"*64)
def projection(**overrides):
    args=dict(subject="https://chatman.ai/cs2#RFC-CS2-001",authority_ceiling="CONSTRUCT",consumer="https://chatman.ai/cs2#ggen-marketplace",work_id="CS2-WRK-003",expected_projection="pinned reusable CS2 pack",provenance=P); args.update(overrides); return ConsumerProjection(**args)
def test_exact_subject_refusal():
    with pytest.raises(SubjectMismatch): projection(subject="https://chatman.ai/cs2#OTHER")
def test_authority_ceiling_refusal():
    with pytest.raises(AuthorityExceeded): projection(authority_ceiling="DO")
def test_materialization_is_deterministic():
    p=projection(); assert materialize([p],"jira")==materialize([p],"jira")
def test_receipt_replays_exact_output():
    p=projection(); m=materialize([p],"a2a"); assert replay_matches(m.receipt,[p],m.rows)
def test_receipt_rejects_changed_output():
    p=projection(); m=materialize([p],"a2a"); assert not replay_matches(m.receipt,[p],m.rows+("changed",))
