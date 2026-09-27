from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FLEET = ROOT / "fleet.ttl"

def test_all_fixture_consumers_share_exact_subject():
    text = FLEET.read_text()
    assert text.count('fa:subjectIri "urn:chatman:rfc:cs2:001"') == 3

def test_binding_keys_are_consumer_specific():
    text = FLEET.read_text()
    for consumer in ("xaas", "gymact", "semantic-jira"):
        assert f'fa:bindingKey "RFC-CS2-001:{consumer}"' in text

def test_every_consumer_declares_output_path():
    text = FLEET.read_text()
    assert text.count("fa:outputPath") == 3
