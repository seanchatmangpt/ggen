import copy
import importlib.util
from pathlib import Path
import unittest

SCRIPT = Path(__file__).with_name("autofde_validate_execution_profiles.py")
SPEC = importlib.util.spec_from_file_location("autofde_validate_execution_profiles", SCRIPT)
validator = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(validator)


def valid_document():
    return {
        "schema": "urn:autofde:execution-profile:v1",
        "generated_by": "ggen:autofde-execution-profile-pack",
        "authority_mode": "external-only",
        "profiles": [
            {
                "profile_id": "memory-counter",
                "provider": "memory",
                "benchmark_revision": "1" * 40,
                "source_ref": "urn:test:source",
                "derived_from": "urn:test:plan",
                "config_json": '{"initial":{"counter":0}}',
                "capability_ref": None,
                "capability_binding": "increment",
                "payload_json": '{"key":"counter","amount":1}',
                "expected_json": '{"counter":1}',
                "input_schema_json": '{"type":"object"}',
                "authority_ref": None,
                "action_ref": "urn:test:action:increment",
            }
        ],
    }


class ExecutionProfileJsonCourtTests(unittest.TestCase):
    def test_valid_profile_is_admitted(self):
        validator.validate_document(valid_document())

    def test_malformed_json_is_refused(self):
        for field in validator.JSON_OBJECT_FIELDS:
            with self.subTest(field=field):
                value = valid_document()
                value["profiles"][0][field] = "{not-json}"
                with self.assertRaisesRegex(validator.ProfileRefusal, "JSON_INVALID"):
                    validator.validate_document(value)

    def test_arrays_scalars_and_null_are_not_object_equivalent(self):
        for lexical in ("[]", '"string"', "1", "true", "null"):
            with self.subTest(lexical=lexical):
                value = valid_document()
                value["profiles"][0]["config_json"] = lexical
                with self.assertRaisesRegex(validator.ProfileRefusal, "MUST_BE_OBJECT"):
                    validator.validate_document(value)

    def test_vacuous_expected_state_is_refused(self):
        value = valid_document()
        value["profiles"][0]["expected_json"] = "{}"
        with self.assertRaisesRegex(validator.ProfileRefusal, "EXPECTED_JSON_VACUOUS"):
            validator.validate_document(value)

    def test_selector_must_be_exactly_one(self):
        for ref, binding in ((None, None), ("urn:test:cap", "increment")):
            with self.subTest(ref=ref, binding=binding):
                value = valid_document()
                value["profiles"][0]["capability_ref"] = ref
                value["profiles"][0]["capability_binding"] = binding
                with self.assertRaisesRegex(validator.ProfileRefusal, "CAPABILITY_SELECTOR"):
                    validator.validate_document(value)

    def test_authority_tokens_are_refused(self):
        for field in validator.FORBIDDEN:
            with self.subTest(field=field):
                value = valid_document()
                value["profiles"][0][field] = "forged"
                with self.assertRaisesRegex(validator.ProfileRefusal, "AUTHORITY_TOKEN_FIELD"):
                    validator.validate_document(value)

    def test_duplicate_profile_identity_is_refused(self):
        value = valid_document()
        value["profiles"].append(copy.deepcopy(value["profiles"][0]))
        with self.assertRaisesRegex(validator.ProfileRefusal, "DUPLICATE_PROFILE_ID"):
            validator.validate_document(value)


if __name__ == "__main__":
    unittest.main()
