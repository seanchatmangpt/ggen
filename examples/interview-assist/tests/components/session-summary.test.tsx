import { describe, it, expect } from "vitest";
import { renderToStaticMarkup } from "react-dom/server";
import { SessionSummary } from "../../components/session-summary";
import type { TransitionReceipt } from "../../lib/domain/receipt";

describe("SessionSummary (real React SSR render)", () => {
  it("displays the exact checksum value when present, never a fabricated one", () => {
    const receipt: TransitionReceipt = {
      used: ["capability/runtime/execute"],
      checksum: { algorithm: "BLAKE3", checksumValue: "abc123" },
    };
    const html = renderToStaticMarkup(<SessionSummary receipt={receipt} />);
    expect(html).toContain("BLAKE3:abc123");
  });

  it("renders an explicit incomplete state, not a blank, when checksum is missing", () => {
    const receipt = {
      used: [],
      checksum: { algorithm: "BLAKE3", checksumValue: "" },
    } as TransitionReceipt;
    const html = renderToStaticMarkup(<SessionSummary receipt={receipt} />);
    expect(html).toContain("Receipt incomplete");
  });
});
