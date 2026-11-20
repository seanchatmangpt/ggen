#!/usr/bin/env python3
"""sjira_control_baseline.py — regenerate the DS-6 control baseline from a live
census of crates/ggen-sjira. Committed baseline must equal this output.
Usage: sjira_control_baseline.py <repo_root> [out_path]
"""
import collections
import glob
import json
import os
import re
import subprocess  # noqa: F401  (unused; census is filesystem-only, never git)
import sys

PCHART_MIN_N = 30
PCHART_MIN_CLASS_N = 5


def census(crate):
    classes = set()
    mention = collections.Counter()
    pattern = re.compile(r'pub enum (\w*Refusal\w*) \{(.*?)\n\}', re.S)
    variant = re.compile(r'\n    ([A-Z]\w*)')
    for f in sorted(glob.glob(os.path.join(crate, "src", "*.rs"))):
        src = open(f).read()
        mod = os.path.basename(f)[:-3]
        for m in pattern.finditer(src):
            for n in variant.findall(m.group(2)):
                classes.add(mod + "." + n)
    suite_tests = {}
    for f in sorted(glob.glob(os.path.join(crate, "tests", "*.rs"))):
        name = os.path.basename(f)
        t = open(f).read()
        suite_tests[name] = len(re.findall(r'#\[test\]', t))
        for cls in classes:
            tok = cls.split(".")[-1]
            c = len(re.findall(r'\b' + tok + r'\b', t))
            if c:
                mention[cls] = mention[cls] + c
    pub_fns = {}
    for f in sorted(glob.glob(os.path.join(crate, "src", "*.rs"))):
        src = open(f).read()
        mod = os.path.basename(f)[:-3]
        pub_fns[mod] = len(re.findall(r'^pub (?:async )?fn ', src, re.M))
    return classes, mention, suite_tests, pub_fns


def main():
    repo = sys.argv[1]
    out = sys.argv[2] if len(sys.argv) > 2 else os.path.join(
        repo, "docs/jira/v26.11.3/baselines/sjira-control-baseline.json")
    crate = os.path.join(repo, "crates", "ggen-sjira")
    classes, mention, suite_tests, pub_fns = census(crate)
    baseline = {
        "schema": "sjira-control-baseline/1",
        "milestone": "v26.11.3",
        "crate": "crates/ggen-sjira",
        "refusal_classes": {
            "class_names": sorted(classes),
            "classes": {k: mention[k] for k in sorted(classes) if mention[k]},
            "total_mentions": sum(mention.values()),
            "spc_rule": (
                "p-chart 3-sigma on class proportion of corpus mentions when "
                "total>=30 and class count>=5; fixed +/-50%% band (label FIXED) "
                "otherwise; never fake sigma"
            ),
            "spc_params": {
                "pchrt_min_n": PCHART_MIN_N,
                "pchart_min_class_n": PCHART_MIN_CLASS_N,
                "fixed_band": 0.5,
            },
        },
        "suites": {
            "count": len(suite_tests),
            "tests_per_suite": dict(sorted(suite_tests.items())),
        },
        "pub_fns": dict(sorted(pub_fns.items())),
        "chain_verification_failures": 0,
        "chain_verification_note": (
            "D5 reserved slot; enforced in CI wiring, local check asserts slot "
            "shape only (see docs/jira/v26.11.3/receipts/ds-6.md)"
        ),
    }
    with open(out, "w") as fh:
        json.dump(baseline, fh, indent=2, sort_keys=True)
        fh.write("\n")
    print("wrote", out, "- classes:", len(baseline["refusal_classes"]["classes"]),
          "mentions:", baseline["refusal_classes"]["total_mentions"],
          "suites:", baseline["suites"]["count"],
          "pub_fns:", sum(pub_fns.values()))
    return 0


if __name__ == "__main__":
    sys.exit(main())
