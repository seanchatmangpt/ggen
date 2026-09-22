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


def production_request():
    return {
        "schema": "autofde.manufacture-request/1",
        "claim_id": "claim-1",
        "rdfdelta": {
            "schema": "autofde.rdfdelta/1",
            "claim_id": "claim-1",
            "adds": [],
            "removes": [],
        },
        "requirement": {
            "name": "sentinel-benign-close",
            "subject": "sentinel-incident",
            "consequence": "sentinel.incident.close",
            "verifier": "azure-arm-postcondition",
            "target_environment": "azure",
            "semantic_types": [],
        },
        "source": {
            "lab_repository": "seanchatmangpt/autofde-lab",
            "lab_revision": LAB_SHA,
        },
        "manufacturer": {
            "repository": "seanchatmangpt/ggen",
            "revision": GGEN_SHA,
        },
        "authority": {"mode": "external-only", "do_authority": False},
    }


def production_manifest(request):
    return {
        "schema": "autofde.capability-bundle-manifest/2",
        "name": "sentinel-benign-close",
        "request_id": manufacturer.raw_sha256(request),
        "lab_revision": LAB_SHA,
        "ggen_revision": GGEN_SHA,
        "consequence": "sentinel.incident.close",
        "artifacts": [
            {
                "path": "bundle/action.json",
                "sha256": "a" * 64,
                "media_type": "application/json",
            },
            {
                "path": "bundle/verifier.json",
                "sha256": "b" * 64,
                "media_type": "application/json",
            },
        ],
    }


def self_consistent_receipt(request, manifest):
    """Build the rejected alternative: hashes agree, semantics may not."""
    receipt = {
        "schema": manufacturer.RECEIPT_SCHEMA,
        "standing": "ALIVE",
        "authority_class": "CONSTRUCT",
        "do_authority": False,
        "validator": manufacturer.VALIDATOR,
        "request_id": manufacturer.raw_sha256(request),
        "request_digest": manufacturer.sha256_json(request),
        "manifest_digest": manufacturer.sha256_json(manifest),
        "artifact_set_digest": manufacturer.artifact_set_digest(manifest),
        "lab_revision": LAB_SHA,
        "ggen_revision": GGEN_SHA,
        "courts": list(manufacturer.REQUIRED_COURTS),
    }
    receipt["receipt_digest"] = manufacturer.receipt_digest(receipt)
    return receipt


class AutoFDEManufactureV2Tests(unittest.TestCase):
    def test_legacy_gall_slice_is_deterministic_and_replay_verified(self):
        req = requirement()
        adm = admission(req)
        first = manufacturer.manufacture(req, adm, GGEN_SHA)
        second = manufacturer.manufacture(req, adm, GGEN_SHA)
        self.assertEqual(first, second)
        payload, receipt = first
        self.assertTrue(
            manufacturer.verify(
                payload,
                receipt,
                expected_ggen_revision=GGEN_SHA,
                expected_lab_revision=LAB_SHA,
                expected_requirement_digest=manufacturer.sha256_json(req),
            )
        )
        self.assertFalse(receipt["do_authority"])

    def test_seal_manifest_binds_request_manifest_artifacts_and_provenance(self):
        request = production_request()
        manifest = production_manifest(request)
        first = manufacturer.seal_manifest(
            request,
            manifest,
            ggen_revision=GGEN_SHA,
            lab_revision=LAB_SHA,
        )
        second = manufacturer.seal_manifest(
            request,
            manifest,
            ggen_revision=GGEN_SHA,
            lab_revision=LAB_SHA,
        )
        self.assertEqual(first, second)
        self.assertTrue(
            manufacturer.verify_manifest_receipt(
                request,
                manifest,
                first,
                expected_ggen_revision=GGEN_SHA,
                expected_lab_revision=LAB_SHA,
            )
        )
        self.assertEqual(first["request_digest"], manufacturer.sha256_json(request))
        self.assertEqual(first["manifest_digest"], manufacturer.sha256_json(manifest))
        self.assertEqual(
            first["artifact_set_digest"], manufacturer.artifact_set_digest(manifest)
        )
        self.assertEqual(
            tuple(sorted(first["courts"])),
            tuple(sorted(manufacturer.REQUIRED_COURTS)),
        )

    def test_receipt_field_tamper_refuses_even_when_manifest_is_unchanged(self):
        request = production_request()
        manifest = production_manifest(request)
        receipt = manufacturer.seal_manifest(
            request,
            manifest,
            ggen_revision=GGEN_SHA,
            lab_revision=LAB_SHA,
        )
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
                    manufacturer.verify_manifest_receipt(
                        request,
                        manifest,
                        forged,
                        expected_ggen_revision=GGEN_SHA,
                        expected_lab_revision=LAB_SHA,
                    )

    def test_self_digest_court_manifest_and_artifact_tamper_refuse(self):
        request = production_request()
        manifest = production_manifest(request)
        receipt = manufacturer.seal_manifest(
            request,
            manifest,
            ggen_revision=GGEN_SHA,
            lab_revision=LAB_SHA,
        )

        forged = copy.deepcopy(receipt)
        forged["courts"] = ["request_binding"]
        forged["receipt_digest"] = manufacturer.receipt_digest(forged)
        with self.assertRaisesRegex(ValueError, "COURTS_INCOMPLETE"):
            manufacturer.verify_manifest_receipt(request, manifest, forged)

        forged = copy.deepcopy(receipt)
        forged["receipt_digest"] = "sha256:" + "0" * 64
        with self.assertRaisesRegex(ValueError, "RECEIPT_DIGEST_MISMATCH"):
            manufacturer.verify_manifest_receipt(request, manifest, forged)

        changed_manifest = copy.deepcopy(manifest)
        changed_manifest["consequence"] = "different"
        with self.assertRaisesRegex(ValueError, "MANIFEST_BINDING_DRIFT"):
            manufacturer.verify_manifest_receipt(request, changed_manifest, receipt)

        changed_manifest = copy.deepcopy(manifest)
        changed_manifest["artifacts"][0]["sha256"] = "f" * 64
        with self.assertRaisesRegex(
            ValueError, "MANIFEST_BINDING_DRIFT|ARTIFACT_SET_BINDING_DRIFT"
        ):
            manufacturer.verify_manifest_receipt(request, changed_manifest, receipt)

    def test_request_revision_authority_and_path_drift_refuse(self):
        request = production_request()
        manifest = production_manifest(request)

        bad = copy.deepcopy(request)
        bad["authority"]["do_authority"] = True
        with self.assertRaisesRegex(ValueError, "AUTHORITY_ESCALATION"):
            manufacturer.seal_manifest(
                bad,
                production_manifest(bad),
                ggen_revision=GGEN_SHA,
                lab_revision=LAB_SHA,
            )

        bad_manifest = copy.deepcopy(manifest)
        bad_manifest["artifacts"][0]["path"] = "../escape.json"
        with self.assertRaisesRegex(ValueError, "ARTIFACT_PATH_INVALID"):
            manufacturer.seal_manifest(
                request,
                bad_manifest,
                ggen_revision=GGEN_SHA,
                lab_revision=LAB_SHA,
            )

        with self.assertRaisesRegex(ValueError, "GGEN_REVISION_INVALID"):
            manufacturer.seal_manifest(
                request,
                manifest,
                ggen_revision="ggen@main",
                lab_revision=LAB_SHA,
            )

    def test_replay_refuses_self_consistent_authority_escalation(self):
        request = production_request()
        request["authority"]["do_authority"] = True
        manifest = production_manifest(request)
        receipt = self_consistent_receipt(request, manifest)

        with self.assertRaisesRegex(ValueError, "REQUEST_AUTHORITY_ESCALATION"):
            manufacturer.verify_manifest_receipt(request, manifest, receipt)

    def test_replay_refuses_self_consistent_request_schema_drift(self):
        request = production_request()
        request["schema"] = "autofde.manufacture-request/999"
        manifest = production_manifest(request)
        receipt = self_consistent_receipt(request, manifest)

        with self.assertRaisesRegex(ValueError, "REQUEST_SCHEMA_INVALID"):
            manufacturer.verify_manifest_receipt(request, manifest, receipt)

    def test_replay_refuses_self_consistent_manifest_provenance_drift(self):
        request = production_request()
        manifest = production_manifest(request)
        manifest["ggen_revision"] = "3" * 40
        receipt = self_consistent_receipt(request, manifest)

        with self.assertRaisesRegex(ValueError, "MANIFEST_PROVENANCE_DRIFT"):
            manufacturer.verify_manifest_receipt(request, manifest, receipt)


if __name__ == "__main__":
    unittest.main()
