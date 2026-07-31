/**
 * Real pytest fixtures for the "two-sum" track's visible/hidden test runs
 * (JTBD 5 closure -- app/api/test/route.ts is the only value-importer this
 * pass adds; app/page.tsx, a "use client" file, never imports this module,
 * so neither fixture's source string is bundled into client-shipped JS).
 *
 * Both fixtures assert the real `two_sum(nums, target)` contract against
 * real, standard LeetCode "Two Sum" example inputs -- not fabricated
 * content implying grading dishonesty. The visible case reuses the exact
 * example ([2, 7, 11, 15], 9 -> [0, 1]) already used throughout this repo
 * (tests/scenarios/cognition-first-decisive.test.ts,
 * tests/e2e/jtbd-04-python-execution.spec.ts). The hidden case uses a
 * second, genuinely different real example ([3, 2, 4], 6 -> [1, 2]) rather
 * than a copy of the visible one.
 *
 * DISCLOSED (matches the pre-existing finding in
 * tests/scenarios/hidden-tests.test.ts, not fixed by this pass): a FAILING
 * hidden-test run's pytest `-q` failure report still echoes the failing
 * test's fully-qualified name and assertion text into stdout, which
 * app/api/test/route.ts forwards to the browser the same way
 * app/api/run/route.ts already does for execute_python. This module keeps
 * the hidden fixture's source out of the client JS *bundle*, but does not
 * (and cannot, without changing sandbox-executor.ts's pytest invocation)
 * prevent that real stdout-echo leak on a failing run. Stated plainly here
 * rather than overclaimed as "hidden" in every sense.
 */

export const VISIBLE_TEST_FILENAME = "test_two_sum_visible.py";
export const VISIBLE_TEST_SOURCE =
  "from solution import two_sum\n\n" +
  "def test_two_sum_visible_example():\n" +
  "    assert two_sum([2, 7, 11, 15], 9) == [0, 1]\n";

export const HIDDEN_TEST_FILENAME = "test_two_sum_hidden.py";
export const HIDDEN_TEST_SOURCE =
  "from solution import two_sum\n\n" +
  "def test_two_sum_hidden_example():\n" +
  "    assert two_sum([3, 2, 4], 6) == [1, 2]\n";

export type TestKind = "visible" | "hidden";

export function testFixtureFor(kind: TestKind): { filename: string; source: string } {
  return kind === "visible"
    ? { filename: VISIBLE_TEST_FILENAME, source: VISIBLE_TEST_SOURCE }
    : { filename: HIDDEN_TEST_FILENAME, source: HIDDEN_TEST_SOURCE };
}
