from pathlib import Path
import json
import re


ROOT = Path(__file__).resolve().parents[1]


def test_canonical_source_declares_three_consumers_and_targets():
    ttl = (ROOT / "canonical.ttl").read_text()
    for consumer in ("ggen-marketplace", "ash-a2a", "xaas"):
        assert f"cs2:{consumer}" in ttl
    for repo in (
        "seanchatmangpt/ggen-marketplace",
        "seanchatmangpt/ash_a2a",
        "seanchatmangpt/xaas",
    ):
        assert repo in ttl


def test_consumer_query_projects_distribution_identity():
    query = (ROOT / "queries" / "consumers.rq").read_text()
    for variable in (
        "?target_repo",
        "?target_path",
        "?artifact_kind",
        "?contract_version",
    ):
        assert variable in query


def test_manifest_wires_all_generated_fleet_outputs():
    manifest = (ROOT / "ggen.toml").read_text()
    outputs = {
        "generated/fleet-contract.json",
        "generated/marketplace-pack.ttl",
        "generated/ash_a2a/generated_fleet_contract.ex",
        "generated/xaas/generated_fleet_contract.ex",
    }
    for output in outputs:
        assert output in manifest


def test_schema_is_bound_to_v1_contract():
    schema = json.loads((ROOT / "schema" / "fleet-contract.schema.json").read_text())
    assert schema["$id"] == "https://chatman.ai/cs2/fleet-contract/v1"
    assert schema["properties"]["contract_schema"]["const"] == schema["$id"]


def test_generated_templates_do_not_embed_alternate_subjects():
    for name in (
        "fleet-contract.json.tera",
        "marketplace-pack.ttl.tera",
        "ash-a2a-fleet-contract.ex.tera",
        "xaas-fleet-contract.ex.tera",
    ):
        text = (ROOT / "templates" / name).read_text()
        assert "RFC-CS2-002" not in text
        assert not re.search(r"CS2-WRK-(?!003|012|013)\\d{3}", text)
