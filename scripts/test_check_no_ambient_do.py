#!/usr/bin/env python3
from __future__ import annotations

import unittest
from pathlib import Path

from scripts.check_no_ambient_do import inspect_source


class NoAmbientDoTests(unittest.TestCase):
    def test_docstring_prose_does_not_manufacture_aws_actuation(self):
        source = '''\ndef validate():\n    """Replay the semantic admission laws that precede manifest sealing."""\n    return True\n'''
        self.assertEqual(inspect_source(source), ())

    def test_real_manufacturer_source_passes(self):
        subject = Path(__file__).with_name("autofde_manufacture_bundle_v2.py")
        self.assertEqual(inspect_source(subject.read_text(encoding="utf-8")), ())

    def test_subprocess_import_is_refused(self):
        findings = inspect_source("import subprocess\nsubprocess.run(['echo', 'x'])\n")
        self.assertTrue(any(row.rule == "forbidden-import" for row in findings))

    def test_os_system_is_refused(self):
        findings = inspect_source("import os\nos.system('echo x')\n")
        self.assertTrue(any(row.symbol == "os.system" for row in findings))

    def test_network_clients_are_refused(self):
        requests_findings = inspect_source("import requests\nrequests.get('https://example.invalid')\n")
        urllib_findings = inspect_source(
            "import urllib.request\nurllib.request.urlopen('https://example.invalid')\n"
        )
        self.assertTrue(requests_findings)
        self.assertTrue(urllib_findings)

    def test_cloud_cli_via_subprocess_is_refused(self):
        for executable in ("aws", "az", "gcloud", "kubectl", "terraform"):
            with self.subTest(executable=executable):
                source = f"import subprocess\nsubprocess.run(['{executable}', '--version'])\n"
                self.assertTrue(inspect_source(source))

    def test_direct_actuation_and_broker_do_are_refused(self):
        findings = inspect_source("engine.actuate(intent)\nbroker.do(intent)\n")
        symbols = {row.symbol for row in findings}
        self.assertIn("engine.actuate", symbols)
        self.assertIn("broker.do", symbols)


if __name__ == "__main__":
    unittest.main()
