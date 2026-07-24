import assert from "node:assert/strict";
import { checkPreconditions, transitiveRequires } from "../preconditions.ts";

// capability/verification/run-complete-test-suite requires both
// run-visible-test and run-hidden-test, which each require runtime/execute,
// which requires runtime/compile, which requires runtime/select-language.
const closure = transitiveRequires("capability/verification/run-complete-test-suite");
console.log("transitiveRequires(run-complete-test-suite) =", closure.sort());
assert.ok(closure.includes("capability/verification/run-visible-test"));
assert.ok(closure.includes("capability/verification/run-hidden-test"));
assert.ok(closure.includes("capability/runtime/execute"));
assert.ok(closure.includes("capability/runtime/compile"));
assert.ok(closure.includes("capability/runtime/select-language"));

const emptyState = new Set();
const unmet = checkPreconditions("capability/runtime/execute", emptyState);
console.log("checkPreconditions(runtime/execute, {}) =", unmet);
assert.equal(unmet.met, false, "execute without prior compile must be unmet");
assert.ok(unmet.missing.includes("capability/runtime/compile"));

const satisfiedState = new Set([
  "capability/runtime/select-language",
  "capability/runtime/compile",
]);
const met = checkPreconditions("capability/runtime/execute", satisfiedState);
console.log("checkPreconditions(runtime/execute, {select-language, compile}) =", met);
assert.equal(met.met, true, "execute with compile satisfied must be met");

console.log("PASS preconditions.test.mjs");
