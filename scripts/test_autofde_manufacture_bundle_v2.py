import copy
import importlib.util
from pathlib import Path
import unittest

SCRIPT = Path(__file__).with_name("autofde_manufacture_bundle_v2.py")
SPEC = importlib.util.spec_from_file_location("autofde_manufacture_bundle_v2", SCRIPT)
manufacturer = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(manufacturer)

LAB_SHA = "1" * 40
GGEN_SHA = "2" * 40


def requirement():
    return {
        "schema": "autofde.engineering-requirement/1",
        "standing": "BLOCKED:CAPABILITY_ABSENT",
        "authority_class": "CONSTRUCT",
        "requirement_id": "req-1",
        "observation_digest": "sha256:" + "a" * 64,
        "capability": "write-greeting",
        "subject": "local:test",
        "consequence": "filesystem.write",
        "idempotency_key": "episode-1",
        "payload": {"kind": "greeting"},
        "manufacture_spec": {
            "kind": "filesystem_write",
            "path": "out/greeting.txt",
            "content": "hello",
            "match_all": {"kind": "greeting"},
        },
    }


def admission(req):
    return {
        "schema": "autofde.lab-admission/1",
        "standing": "ALIVE",
        "authority_class": "CONSTRUCT",
        "do_authority": False,
        "requirement_id": req["requirement_id"],
        "requirement_digest": manufacturer.sha256_json(req),
        "capability": req["capability"],
        "lab_revision": LAB_SHA,
        "powl_digest": "sha256:" + "b" * 64,
        "admission_digest": "sha256:" + "c" * 64,
    }


class AutoFDEManufactureV2Tests(unittest.TestCase):
    def make(self):
        req = requirement()
        adm = admission(req)
        payload, receipt = manufacturer.manufacture(req, adm, GGEN_SHA)
        return req, adm, payload, receipt

    def test_deterministic_replay_verifies_all_bindings(self):
        req, adm, payload, receipt = self.make()
        again = manufacturer.manufacture(req, adm, GGEN_SHA)
        self.assertEqual((payload, receipt), again)
        self.assertTrue(manufacturer.verify(payload, receipt, expected_ggen_revision=GGEN_SHA, expected_lab_revision=LAB_SHA, expected_requirement_digest=manufacturer.sha256_json(req)))
        self.assertFalse(receipt["do_authority"])
        self.assertEqual(receipt["authority_class"], "CONSTRUCT")
        self.assertEqual(tuple(sorted(receipt["courts"])), tuple(sorted(manufacturer.REQUIRED_COURTS)))

    def test_receipt_field_tamper_refuses_even_when_payload_is_unchanged(self):
        _, _, payload, receipt = self.make()
        for field, bad in (
            ("ggen_revision", "3" * 40),
            ("lab_revision", "4" * 40),
            ("authority_class", "DO"),
            ("do_authority", True),
            ("validator", "other"),
            ("standing", "UNKNOWN"),
        ):
            with self.subTest(field=field):
                forged = copy.deepcopy(receipt)
                forged[field] = bad
                with self.assertRaises(ValueError):
                    manufacturer.verify(payload, forged, expected_ggen_revision=GGEN_SHA, expected_lab_revision=LAB_SHA)

    def test_self_digest_and_court_tamper_refuse(self):
        _, _, payload, receipt = self.make()
        forged = copy.deepcopy(receipt)
        forged["courts"] = ["canonical_sha256"]
        forged["receipt_digest"] = manufacturer.receipt_digest(forged)
        with self.assertRaisesRegex(ValueError, "COURTS_INCOMPLETE"):
            manufacturer.verify(payload, forged)

        forged = copy.deepcopy(receipt)
        forged["receipt_digest"] = "sha256:" + "0" * 64
        with self.assertRaisesRegex(ValueError, "RECEIPT_DIGEST_MISMATCH"):
            manufacturer.verify(payload, forged)

    def test_payload_tamper_and_verifier_divergence_refuse(self):
        _, _, payload, receipt = self.make()
        changed = copy.deepcopy(payload)
        changed["program"]["content"] = "evil"
        with self.assertRaisesRegex(ValueError, "BUNDLE_DIGEST_MISMATCH"):
            manufacturer.verify(changed, receipt)

        changed = copy.deepcopy(payload)
        changed["verifier"]["digest"] = "0" * 64
        forged_receipt = copy.deepcopy(receipt)
        forged_receipt["bundle_digest"] = manufacturer.raw_sha256(changed)
        forged_receipt["receipt_digest"] = manufacturer.receipt_digest(forged_receipt)
        with self.assertRaisesRegex(ValueError, "PROGRAM_VERIFIER_NOT_CLOSED"):
            manufacturer.verify(changed, forged_receipt)

    def test_requirement_and_revision_drift_refuse_before_manufacture(self):
        req = requirement()
        adm = admission(req)
        req["capability"] = "changed"
        with self.assertRaisesRegex(ValueError, "DIGEST_DRIFT"):
            manufacturer.manufacture(req, adm, GGEN_SHA)
        with self.assertRaisesRegex(ValueError, "GGEN_REVISION_INVALID"):
            manufacturer.manufacture(requirement(), admission(requirement()), "ggen@main")


if __name__ == "__main__":
    unittest.main()
