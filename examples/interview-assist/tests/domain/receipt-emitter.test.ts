// TICKET-055: emitReceipt unit tests. Chicago TDD -- exercises the real
// BLAKE3 adapter (checksum-adapter.ts -> the real `blake3` npm package),
// nothing mocked.
import { describe, it, expect } from "vitest";
import { emitReceipt } from "../../lib/domain/receipt-emitter";

describe("emitReceipt (TICKET-055, real BLAKE3 hashing)", () => {
  it("emits a receipt with a real 64-hex-char BLAKE3 checksum", () => {
    const receipt = emitReceipt("admission", {
      used: ["SessionEvent"],
      timestamp: 1_700_000_000_000,
    });
    expect(receipt.checksum.algorithm).toBe("BLAKE3");
    expect(receipt.checksum.checksumValue).toMatch(/^[0-9a-f]{64}$/);
    expect(receipt.used).toEqual(["SessionEvent"]);
    expect(receipt.label).toBe("admission");
    expect(receipt.derivedFrom).toBeUndefined();
    expect(receipt.relation).toBeUndefined();
  });

  it("is deterministic: identical inputs (including timestamp) produce an identical checksum", () => {
    const a = emitReceipt("sandbox-execution", { used: ["execute_python"], timestamp: 42 });
    const b = emitReceipt("sandbox-execution", { used: ["execute_python"], timestamp: 42 });
    expect(a.checksum.checksumValue).toBe(b.checksum.checksumValue);
  });

  it("produces a different checksum when the timestamp differs (no fabricated collision)", () => {
    const a = emitReceipt("sandbox-execution", { used: ["execute_python"], timestamp: 1 });
    const b = emitReceipt("sandbox-execution", { used: ["execute_python"], timestamp: 2 });
    expect(a.checksum.checksumValue).not.toBe(b.checksum.checksumValue);
  });

  it("produces a different checksum when the step differs, same data", () => {
    const a = emitReceipt("sandbox-execution", { used: ["x"], timestamp: 1 });
    const b = emitReceipt("test-result", { used: ["x"], timestamp: 1 });
    expect(a.checksum.checksumValue).not.toBe(b.checksum.checksumValue);
  });

  it("chains: derivedFrom/relation point at the prior receipt's checksum, and chaining changes the checksum", () => {
    const first = emitReceipt("admission", { used: ["SessionEvent"], timestamp: 1 });
    const second = emitReceipt("sandbox-execution", {
      used: ["execute_python"],
      timestamp: 2,
      prevReceipt: first,
    });
    expect(second.derivedFrom).toBe(first.checksum.checksumValue);
    expect(second.relation).toBe(first.checksum.checksumValue);

    const secondUnchained = emitReceipt("sandbox-execution", { used: ["execute_python"], timestamp: 2 });
    expect(second.checksum.checksumValue).not.toBe(secondUnchained.checksum.checksumValue);
  });

  it("canonicalizes independently of object key order (same logical payload -> same checksum)", () => {
    // emitReceipt's own `data` parameter has a fixed field order in this
    // test file regardless, so this asserts the underlying canonicalizer
    // property directly: two calls whose only difference is JS engine
    // enumeration order of an equivalent literal must still agree.
    const a = emitReceipt("admission", { timestamp: 5, used: ["A", "B"], label: "L" });
    const b = emitReceipt("admission", { label: "L", used: ["A", "B"], timestamp: 5 });
    expect(a.checksum.checksumValue).toBe(b.checksum.checksumValue);
  });

  it("covers all 4 real manufacturing-chain steps from 60-provenance-receipts.ttl", () => {
    const steps = ["admission", "sandbox-execution", "test-result", "accessibility-projection"] as const;
    const checksums = steps.map(
      (step) => emitReceipt(step, { used: ["probe"], timestamp: 99 }).checksum.checksumValue,
    );
    // all real, all distinct (step name is part of the hashed payload)
    for (const c of checksums) expect(c).toMatch(/^[0-9a-f]{64}$/);
    expect(new Set(checksums).size).toBe(4);
  });

  it("covers the 5th step (cognition-run, from 90-cognition-bridge.ttl's <manufacturing-chain/cognition-activity>, Phase 3)", () => {
    const steps = [
      "admission",
      "cognition-run",
      "sandbox-execution",
      "test-result",
      "accessibility-projection",
    ] as const;
    const checksums = steps.map(
      (step) => emitReceipt(step, { used: ["probe"], timestamp: 99 }).checksum.checksumValue,
    );
    for (const c of checksums) expect(c).toMatch(/^[0-9a-f]{64}$/);
    expect(new Set(checksums).size).toBe(5);
  });
});
