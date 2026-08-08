import importlib.util
from pathlib import Path
import unittest

SCRIPT = Path(__file__).with_name("autofde_manufacture_bundle.py")
SPEC = importlib.util.spec_from_file_location("autofde_manufacture_bundle", SCRIPT)
manufacturer = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(manufacturer)


def requirement():
    return {
        "schema": "autofde.engineering-requirement/1",
        "standing": "BLOCKED:CAPABILITY_ABSENT",
        "authority_class": "CONSTRUCT",
        "requirement_id": "req-1",
        "observation_digest": "sha256:abc",
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
        "lab_revision": "lab@abc",
        "powl_digest": "sha256:def",
        "admission_digest": "sha256:admit",
    }


class AutoFDEManufactureTests(unittest.TestCase):
    def test_deterministic_and_closed(self):
        req = requirement()
        adm = admission(req)
        first = manufacturer.manufacture(req, adm, "ggen@abc")
        second = manufacturer.manufacture(req, adm, "ggen@abc")
        self.assertEqual(first, second)
        manufacturer.verify(*first)
        self.assertFalse(first[1]["do_authority"])

    def test_requirement_drift_is_refused(self):
        req = requirement()
        adm = admission(req)
        req["capability"] = "changed"
        with self.assertRaisesRegex(ValueError, "DIGEST_DRIFT"):
            manufacturer.manufacture(req, adm, "ggen@abc")


if __name__ == "__main__":
    unittest.main()
