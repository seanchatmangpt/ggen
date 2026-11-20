#!/usr/bin/env python3
"""sjira_control_plan.py — DS-6 control plan for the ggen-sjira admission kernel.

Detects five drift classes against the committed baseline
docs/jira/v26.11.3/baselines/sjira-control-baseline.json:

  D1 refusal-class mix shift   - new/vanished typed refusal classes, plus
    per-class proportion checks: p-chart 3-sigma limits where corpus n is real
    (baseline total >= 30), fixed bounds (labeled FIXED) for small-n classes.
  D2 suite-count regression    - tests/*.rs count below baseline (removal = drift).
  D3 per-suite test-count regression - #[test] count per suite below baseline.
  D4 API-surface drift         - pub fn count per module below baseline (vanishing).
  D5 chain-verification slot   - reserved metric; baseline slot must exist and be
    an integer >= 0; CI enforcement is wiring (see receipts/ds-6.md).

Exit 0 = in control. Exit 1 = drift; one typed VIOLATION line per finding.
Never runs git. Usage: sjira_control_plan.py <repo_root> [baseline.json]
"""
import collections
import glob
import json
import math
import os
import re
import sys

PCHART_MIN_N = 30
PCHART_MIN_CLASS_N = 5
FIXED_BAND = 0.5


def census(crate):
    live_classes = set()
    mention = collections.Counter()
    pattern = re.compile(r'pub enum (\w*Refusal\w*) \{(.*?)\n\}', re.S)
    variant = re.compile(r'\n    ([A-Z]\w*)')
    token_re_cache = {}
    for f in sorted(glob.glob(os.path.join(crate, "src", "*.rs"))):
        src = open(f).read()
        mod = os.path.basename(f)[:-3]
        for m in pattern.finditer(src):
            for n in variant.findall(m.group(2)):
                live_classes.add(mod + "." + n)
    suite_tests = {}
    for f in sorted(glob.glob(os.path.join(crate, "tests", "*.rs"))):
        name = os.path.basename(f)
        t = open(f).read()
        suite_tests[name] = len(re.findall(r'#\[test\]', t))
        for cls in live_classes:
            token = cls.split(".")[-1]
            if token not in token_re_cache:
                token_re_cache[token] = re.compile(r'\b' + token + r'\b')
            c = len(token_re_cache[token].findall(t))
            if c:
                mention[cls] = mention[cls] + c
    pub_fns = {}
    for f in sorted(glob.glob(os.path.join(crate, "src", "*.rs"))):
        src = open(f).read()
        mod = os.path.basename(f)[:-3]
        pub_fns[mod] = len(re.findall(r'^pub (?:async )?fn ', src, re.M))
    return live_classes, mention, suite_tests, pub_fns


def main():
    repo = sys.argv[1]
    if len(sys.argv) > 2:
        baseline_path = sys.argv[2]
    else:
        baseline_path = os.path.join(
            repo, "docs/jira/v26.11.3/baselines/sjira-control-baseline.json")
    crate = os.path.join(repo, "crates", "ggen-sjira")
    violations = []

    def violate(code, detail):
        violations.append("VIOLATION class=" + code + " " + detail)

    with open(baseline_path) as fh:
        baseline = json.load(fh)
    live_classes, mention, suite_tests, pub_fns = census(crate)

    b_ref = baseline["refusal_classes"]
    base_classes = set(b_ref.get("class_names", b_ref["classes"].keys()))
    for cls in sorted(live_classes - base_classes):
        violate("D1-new-refusal-class",
                "new typed refusal class not in baseline: " + cls)
    for cls in sorted(base_classes - live_classes):
        violate("D1-vanished-refusal-class",
                "baseline refusal class absent from src: " + cls)

    base_total = b_ref["total_mentions"]
    live_total = sum(mention.values())
    for cls in sorted(base_classes):
        bcount = b_ref["classes"].get(cls, 0)
        p = bcount / base_total
        if base_total >= PCHART_MIN_N and bcount >= PCHART_MIN_CLASS_N:
            sigma = math.sqrt(p * (1 - p) / base_total)
            lo = p - 3 * sigma
            if lo < 0.0:
                lo = 0.0
            hi = p + 3 * sigma
            kind = "PCHART-3SIGMA"
        else:
            lo = FIXED_BAND * p
            hi = (1 + FIXED_BAND) * p
            kind = "FIXED"
        cur = mention.get(cls, 0)
        cur_p = 0.0
        if live_total:
            cur_p = cur / live_total
        if cur_p < lo or cur_p > hi:
            violate("D1-proportion-out-of-control",
                    cls + " baseline_p=%.4f current_p=%.4f limits=[%.4f,%.4f] rule=%s"
                    % (p, cur_p, lo, hi, kind))

    base_suites = baseline["suites"]["count"]
    if len(suite_tests) < base_suites:
        violate("D2-suite-count-regression",
                "suites %d < baseline %d" % (len(suite_tests), base_suites))

    for name in sorted(baseline["suites"]["tests_per_suite"]):
        cnt = baseline["suites"]["tests_per_suite"][name]
        if name in suite_tests and suite_tests[name] < cnt:
            violate("D3-test-count-regression",
                    "suite %s: %d < baseline %d" % (name, suite_tests[name], cnt))

    for mod in sorted(baseline["pub_fns"]):
        cnt = baseline["pub_fns"][mod]
        if mod in pub_fns and pub_fns[mod] < cnt:
            violate("D4-pubfn-vanishing",
                    "module %s: %d < baseline %d" % (mod, pub_fns[mod], cnt))
    for mod in sorted(set(baseline["pub_fns"].keys()) - set(pub_fns.keys())):
        violate("D4-pubfn-vanishing", "module %s vanished from src/" % mod)

    chain = baseline.get("chain_verification_failures")
    if not isinstance(chain, int) or isinstance(chain, bool) or chain < 0:
        violate("D5-chain-slot-invalid",
                "baseline chain_verification_failures=%r not int>=0" % (chain,))

    for v in violations:
        print(v)
    if violations:
        print("sjira-control-plan: OUT OF CONTROL (%d violations)" % len(violations))
        return 1
    print("sjira-control-plan: IN CONTROL (%d refusal classes, %d corpus mentions, "
          "%d suites, %d pub fns)" % (len(base_classes), live_total,
                                      len(suite_tests), sum(pub_fns.values())))
    return 0


if __name__ == "__main__":
    sys.exit(main())
