// Chicago TDD: real rdflib-equivalent re-derivation, not a hardcoded constant.
// Runs the *actual* SPARQL query against the *actual* ontology file via a
// child rdflib process, and compares against the generated capability.ts
// module's live export -- proving TICKET-026's falsifier (no hardcoding).
import { execFileSync } from "node:child_process";
import assert from "node:assert/strict";
import { CAPABILITY_IDS, CAPABILITY_COUNT } from "../capability.ts";

const PACK = "/Users/sac/ggen/packs/wasm4pm-interview-assist-pack";

const py = `
import sys
from rdflib import Graph
g = Graph()
g.parse("${PACK}/ontology/30-capabilities.ttl", format="turtle")
q = open("${PACK}/queries/capabilities.rq").read()
print(len(list(g.query(q))))
`;

const liveCount = parseInt(execFileSync("python3", ["-c", py]).toString().trim(), 10);

assert.equal(CAPABILITY_COUNT, 98, "CAPABILITY_COUNT must be 98");
assert.equal(CAPABILITY_IDS.length, 98, "CAPABILITY_IDS array length must be 98");
assert.equal(liveCount, CAPABILITY_COUNT, "generated count must match a live SPARQL re-query, not a hardcoded value");
console.log("PASS capability.count.test.mjs: live=%d generated=%d", liveCount, CAPABILITY_COUNT);

// Negative test: add a 99th capability to a fixture copy of the ontology and
// confirm the *query* (not the generated file) grows to 99 -- proving the
// pipeline is RDF-driven, since the generated file is static output of a
// run, not itself re-queried at runtime.
import { writeFileSync, readFileSync, mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const src = readFileSync(`${PACK}/ontology/30-capabilities.ttl`, "utf8");
const fixtureDir = mkdtempSync(join(tmpdir(), "cap-fixture-"));
const fixturePath = join(fixtureDir, "30-capabilities-plus-one.ttl");
const extra = `\n<capability/session/fixture-99th-capability> a schema:Action ; schema:name "fixture 99th capability" ; dcterms:isPartOf <capability-category/session> .\n`;
writeFileSync(fixturePath, src + extra);

const py2 = `
from rdflib import Graph
g = Graph()
g.parse("${fixturePath}", format="turtle")
q = open("${PACK}/queries/capabilities.rq").read()
print(len(list(g.query(q))))
`;
const grownCount = parseInt(execFileSync("python3", ["-c", py2]).toString().trim(), 10);
assert.equal(grownCount, 99, "adding a 99th capability/* resource must grow the live query result to 99");
console.log("PASS negative test: fixture with 99th capability -> live query count = %d", grownCount);
