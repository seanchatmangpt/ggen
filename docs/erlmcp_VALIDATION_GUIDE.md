# erlmcp Empirical Validation Guide

**Purpose**: Actually run erlmcp tests and benchmarks to validate ALL claims with real data.

## The Problem

I made the critical error of analyzing erlmcp by reading code and documentation without running it. This produced unvalidated claims about:
- ❌ "43,000 msg/s TCP performance"
- ❌ "200+ tests passing"
- ❌ "87% coverage"
- ❌ All other performance claims

## The Solution: Use Claude Code on the Web

Instead of being constrained by local environment, use cloud infrastructure to actually run the tests.

## How to Validate (Step by Step)

### Option 1: Use `&` Command from Claude Code Terminal

If you have Claude Code installed locally:

```bash
claude

# Then in the Claude Code session:
& Clone https://github.com/seanchatmangpt/erlmcp and run the full validation:
1. rebar3 compile - verify it compiles
2. rebar3 eunit - count actual tests and pass/fail
3. rebar3 cover - measure actual coverage percentage
4. make bench-core-ops - get registry msg/s
5. make bench-network-real - validate 43K msg/s TCP claim
6. make bench-stress - validate 372K msg/s sustained claim
7. make bench-integration - measure end-to-end latency

Create EMPIRICAL_VALIDATION_RESULTS.md with:
- Compilation output (success/failure)
- Exact test count (not "200+", actual number)
- Actual coverage % (not "87%", measured value)
- Real benchmark numbers (actual msg/s from stdout)
- All log files as evidence

NO DOCUMENTATION READING - ONLY EXECUTION RESULTS.
```

### Option 2: Use Web Interface

1. Go to https://claude.ai/code
2. Select erlmcp repository
3. Paste the validation task:

```
Empirically validate all erlmcp performance and test claims:

SETUP:
- Verify Erlang and rebar3 are available
- If not, install them (cloud environment should have them)

COMPILE:
```bash
rebar3 compile 2>&1 | tee compile.log
echo "Result: $([ $? -eq 0 ] && echo 'SUCCESS' || echo 'FAILED')"
```

TESTS:
```bash
rebar3 eunit 2>&1 | tee tests.log
grep -E "Finished in|tests passed|tests failed" tests.log
```

COVERAGE:
```bash
rebar3 cover 2>&1 | tee coverage.log
grep "total" coverage.log
```

BENCHMARKS:
```bash
make bench-core-ops 2>&1 | tee bench_core.log
make bench-network-real 2>&1 | tee bench_network.log
make bench-stress 2>&1 | tee bench_stress.log
grep -E "msg/s|ops/s" bench_*.log
```

Create EMPIRICAL_VALIDATION_RESULTS.md with ALL actual numbers.
```

## Critical Claims to Validate

| Claim | Source | Validation Method | Expected Output |
|-------|--------|-------------------|-----------------|
| "43,000 msg/s TCP" | Documentation | `make bench-network-real` | Actual msg/s number |
| "200+ tests" | Documentation | `rebar3 eunit` | Exact test count |
| "87% coverage" | Documentation | `rebar3 cover` | Actual coverage % |
| "553K msg/s registry" | Documentation | `make bench-core-ops` | Actual ops/s |
| "372K msg/s sustained" | Documentation | `make bench-stress` | Actual sustained rate |
| "Code compiles" | Assumption | `rebar3 compile` | Success/failure |

## What Success Looks Like

After running validation, you'll have:

```
EMPIRICAL_VALIDATION_RESULTS.md:
  ✓ Compilation: SUCCESS (or FAILED with errors)
  ✓ Tests: 247 run, 247 passed, 0 failed (actual numbers)
  ✓ Coverage: 84.2% (actual measured %)
  ✓ TCP throughput: 41,234 msg/s (actual benchmark result)
  ✓ Registry ops: 487,392 ops/s (actual benchmark result)
  ✓ Sustained load: 315,678 msg/s over 30s (actual benchmark result)

  Evidence files:
  - compile.log (full compilation output)
  - tests.log (complete test run)
  - coverage.log (coverage report)
  - bench_*.log (all benchmark outputs)
```

## Why This Matters

**Before**: "erlmcp appears to have 200+ tests based on counting test files"
**After**: "erlmcp has 247 tests, all passing, measured coverage is 84.2%"

**Before**: "Documentation claims 43K msg/s TCP performance"
**After**: "Actual benchmark: 41,234 msg/s TCP throughput under 4KB packet load"

**The difference**: EMPIRICAL DATA vs DOCUMENTATION CLAIMS

## Next Steps

1. Run the validation (choose Option 1 or 2 above)
2. Collect the EMPIRICAL_VALIDATION_RESULTS.md
3. Update the erlmcp compliance report with ACTUAL numbers
4. Commit validated findings to ggen repository
5. Mark validation as COMPLETE with evidence

## Lessons Applied

- ✅ Never trust documentation without validation
- ✅ Use available tools (Claude Code on web) to overcome constraints
- ✅ Think outside the box (cloud execution vs local limitations)
- ✅ Provide evidence, not claims
- ✅ Distinguish between "appears" (code review) and "is" (empirical)

---

**Created**: 2026-01-30
**Purpose**: Correct previous unvalidated analysis
**Method**: Empirical testing via Claude Code on the web
**Status**: Ready to execute
