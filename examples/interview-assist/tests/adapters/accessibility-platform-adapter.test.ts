import { describe, it, expect } from "vitest";
import { ariaLiveForSeverity, buildAnnouncement } from "../../lib/adapters/accessibility-platform-adapter";

describe("accessibility-platform-adapter (DOM-free logic only)", () => {
  it("maps refusal/warning to assertive live regions", () => {
    expect(ariaLiveForSeverity("refusal")).toBe("assertive");
    expect(ariaLiveForSeverity("warning")).toBe("assertive");
  });

  it("maps info to polite live regions", () => {
    expect(ariaLiveForSeverity("info")).toBe("polite");
  });

  it("builds a real announcement object after the policy check passes", () => {
    const a = buildAnnouncement("refusal", "Execution refused: timeout exceeded");
    expect(a.ariaLive).toBe("assertive");
    expect(a.text).toBe("Execution refused: timeout exceeded");
  });

  it("NOT TESTED HERE: announceToLiveRegion (real DOM) and speak (real " +
     "Web Speech API) -- jsdom has no SpeechSynthesis polyfill; both " +
     "require Playwright against a real Chromium instance, documented " +
     "rather than mocked", () => {
    expect(true).toBe(true);
  });
});
