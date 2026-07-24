// Chicago TDD: real generated policy-check.ts against real RDF-derived data.
// Safety-critical falsifier (TICKET-028): prohibited-mode must deny all 6
// prohibited-action/* resources. Positive path: practice-mode must allow.
import assert from "node:assert/strict";
import { checkPolicy, POLICY_STATEMENTS } from "../policy-check.ts";

const PROHIBITED_ACTIONS = [
  "prohibited-action/hidden-overlay",
  "prohibited-action/screen-capture-evasion",
  "prohibited-action/monitoring-bypass",
  "prohibited-action/disguised-process",
  "prohibited-action/covert-audio-capture",
  "prohibited-action/misrepresent-unaided-work",
];

assert.equal(
  POLICY_STATEMENTS["policy/prohibited-mode"].filter((s) => s.kind === "prohibition").length,
  6,
  "policy/prohibited-mode must have exactly 6 odrl:Prohibition statements"
);

console.log("=== SAFETY-CRITICAL TEST: policy/prohibited-mode denial ===");
for (const action of PROHIBITED_ACTIONS) {
  const decision = checkPolicy(action, "policy/prohibited-mode");
  console.log(`checkPolicy("${action}", "policy/prohibited-mode") -> ${decision}`);
  assert.equal(decision, "denied", `${action} MUST be denied under policy/prohibited-mode`);
}
console.log("PASS: all 6 prohibited-action/* resources denied under policy/prohibited-mode");

console.log("\n=== POSITIVE-PATH TEST: policy/practice-mode allowance ===");
const practiceDecision = checkPolicy("authority-action/execute-code", "policy/practice-mode");
console.log(`checkPolicy("authority-action/execute-code", "policy/practice-mode") -> ${practiceDecision}`);
assert.equal(practiceDecision, "allowed", "authority-action/execute-code must be allowed under policy/practice-mode");
console.log("PASS: practice-mode allows execute-code (checker is not fail-closed-always)");
