/**
 * JTBD 11: a SECOND, independently real track ("valid-parentheses") produces
 * its own real Eliza-proposed explanation end to end through the real WASM
 * call -- not a copy-paste of the Two Sum demo, and not simulated.
 *
 * Same real network round trip as jtbd-01-cognition-proposal.spec.ts (real
 * POST /api/cognition -> real wasm4pm-cognition Eliza breed, server-side, no
 * mock), but for the second track added to
 * packs/wasm4pm-interview-assist-pack/ontology/90-cognition-bridge.ttl's
 * Part B (BALANCED/BRACKETS/PARENTHESES/STACK keywords, disjoint from the
 * Two Sum track's ARRAY/TARGET/INDICES/SUM). Proves two things a single-track
 * catalog can't: (1) the generated rule set now spans more than one track id,
 * and (2) an utterance built from the new track's own keywords resolves to
 * one of ITS conclusions, never one of Two Sum's.
 */
import { test, expect, submitUtterance } from "./support";
import { COGNITION_RULES, COGNITION_TARGET_TRACKS } from "../../lib/domain/cognition-rules";

const TWO_SUM_KEYWORDS = ["ARRAY", "TARGET", "INDICES", "SUM"];
const VALID_PARENTHESES_KEYWORDS = ["PARENTHESES", "BRACKETS", "STACK", "BALANCED"];

test("the generated rule catalog now spans two distinct track ids", () => {
  expect(COGNITION_TARGET_TRACKS).toContain("two-sum");
  expect(COGNITION_TARGET_TRACKS).toContain("valid-parentheses");

  const validParenthesesRules = COGNITION_RULES.filter((r) => r.id.startsWith("valid-parentheses-"));
  expect(validParenthesesRules).toHaveLength(4);
  expect(validParenthesesRules.map((r) => r.premise[0]).sort()).toEqual([...VALID_PARENTHESES_KEYWORDS].sort());
});

test("submitting a Valid-Parentheses-pattern utterance produces a real, distinct Eliza-proposed track", async ({
  page,
  pageErrors,
}) => {
  await page.goto("/");

  const utterance = "Given a string of parentheses and brackets, check whether they are properly matched";
  const { response, body } = await submitUtterance(page, utterance);

  // Real network evidence: a 200 (matched) response from the real route.
  expect(response.status()).toBe(200);
  expect(body.status).toBe("matched");

  // The real WASM output picked a real valid-parentheses rule -- one of the
  // 4 new conclusions, never a Two Sum conclusion (proves the two tracks
  // are genuinely distinct, not the same content under two labels).
  const validParenthesesConclusions = COGNITION_RULES.filter((r) =>
    r.id.startsWith("valid-parentheses-"),
  ).map((r) => r.conclusion);
  const twoSumConclusions = COGNITION_RULES.filter((r) => r.id.startsWith("two-sum-")).map((r) => r.conclusion);

  expect(validParenthesesConclusions).toContain(body.explanation);
  expect(twoSumConclusions).not.toContain(body.explanation);
  expect(VALID_PARENTHESES_KEYWORDS).toContain(body.selected);
  expect(TWO_SUM_KEYWORDS).not.toContain(body.selected);

  // The exact same real explanation text renders in the DOM.
  const question = page.getByTestId("cognition-panel-question");
  await expect(question).toHaveText(body.explanation as string);

  // The observed utterance itself is echoed back verbatim.
  await expect(page.getByTestId("cognition-panel-intent")).toContainText(utterance);

  // Confirm/reject controls are present for a real match.
  await expect(page.getByTestId("cognition-panel-confirm")).toBeVisible();
  await expect(page.getByTestId("cognition-panel-reject")).toBeVisible();

  expect(pageErrors).toHaveLength(0);
});

test("the STACK-keyword utterance deterministically selects the real valid-parentheses-stack rule", async ({
  page,
}) => {
  await page.goto("/");
  const { body } = await submitUtterance(page, "Would a stack help me solve this problem?");
  expect(body.status).toBe("matched");
  expect(body.selected).toBe("STACK");
  const stackRule = COGNITION_RULES.find((r) => r.id === "valid-parentheses-stack");
  expect(stackRule).toBeDefined();
  expect(body.explanation).toBe(stackRule!.conclusion);
  await expect(page.getByTestId("cognition-panel-matched")).toHaveAttribute("data-selected", "STACK");
});

test("a session can move from a Two Sum match to a Valid Parentheses match across two real turns", async ({
  page,
  pageErrors,
}) => {
  await page.goto("/");

  const first = await submitUtterance(page, "I have an array of numbers to search through");
  expect(first.body.status).toBe("matched");
  expect(TWO_SUM_KEYWORDS).toContain(first.body.selected);

  // Reject the first proposal so the page returns to an idle input state
  // (real reducer transition, not a page reload) before submitting the
  // second, unrelated utterance -- proving the same live session can
  // recognize either track, not just whichever one it saw first.
  await page.getByTestId("cognition-panel-reject").click();

  // Deliberately contains only the BALANCED keyword (not "brackets" --
  // frame.rs's real keyword engine matches the FIRST matching token in
  // the intent, so an utterance containing two registered keywords would
  // deterministically resolve to whichever appears first, not necessarily
  // "balanced"; this utterance avoids that ambiguity on purpose).
  const second = await submitUtterance(page, "Is the sequence balanced overall?");
  expect(second.body.status).toBe("matched");
  expect(VALID_PARENTHESES_KEYWORDS).toContain(second.body.selected);
  expect(second.body.selected).toBe("BALANCED");

  expect(pageErrors).toHaveLength(0);
});
