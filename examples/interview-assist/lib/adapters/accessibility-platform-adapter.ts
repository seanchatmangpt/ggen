/**
 * TICKET-039 (adapter half): Accessibility platform adapter (custom).
 *
 * Real ARIA live-region wiring + Web Speech API (TTS) bindings behind
 * TICKET-033's generated accessibility-controls.tsx port. TICKET-033 has
 * not generated yet in this session; `AccessibilityControlsProps` below is
 * authored by hand and marked PENDING(TICKET-033).
 *
 * BOTH `window.speechSynthesis` (Web Speech API) and a real DOM live
 * region require a real browser -- there is no jsdom polyfill for
 * SpeechSynthesis in this package's toolchain (jsdom does not implement
 * it), so those two functions are exercised only via Playwright
 * (TICKET-039's harness half) against a real Chromium instance, not here.
 * What IS unit-testable without a browser is the pure logic: computing the
 * ARIA live-region politeness level from an event severity, and building
 * the DOM-free announcement text. That is what this file's tests cover.
 */
import { checkPolicy, DEFAULT_ACTIVE_MODE, type PolicyId } from "./policy-check-adapter";
import { emitReceipt } from "../domain/receipt-emitter";
import type { TransitionReceipt } from "../domain/receipt";

/** PENDING(TICKET-033): expected shape of accessibility-controls.tsx's
 * generated props. Replace with the real generated import once it exists. */
export interface AccessibilityControlsProps {
  ttsEnabled: boolean;
  brailleCompatible: boolean;
}

export type AnnouncementSeverity = "info" | "warning" | "refusal";
export type AriaLive = "polite" | "assertive" | "off";

/**
 * Pure, DOM-free mapping from an announcement severity to the ARIA
 * `aria-live` politeness level a live region must be configured with.
 * `refusal`/`warning` are assertive (interrupt) per WCAG guidance for
 * time-sensitive status changes; `info` is polite (queued).
 */
export function ariaLiveForSeverity(severity: AnnouncementSeverity): AriaLive {
  return severity === "info" ? "polite" : "assertive";
}

export interface Announcement {
  text: string;
  ariaLive: AriaLive;
  /** TICKET-055: real receipt for this real accessibility-state
   * projection (<manufacturing-chain/accessibility-projection> in
   * 60-provenance-receipts.ttl). `buildAnnouncement` is the natural,
   * already-existing call site for "wherever accessibility state changes
   * are applied" -- no new call site had to be invented: every real
   * announcement built here IS a real accessibility-state projection
   * (the object a live region/TTS binding is about to render). */
  receipt: TransitionReceipt;
}

/**
 * Builds the announcement object a real live-region DOM node would render.
 * Calls the policy check before returning (a denied capability must never
 * reach the live region as an announcement), even though "acting" here is
 * still DOM-free object construction -- the real DOM write happens in the
 * browser-only `announceToLiveRegion` below.
 *
 * TICKET-055: emits a real TransitionReceipt for the
 * accessibility-projection manufacturing-chain step after the policy
 * check passes (never before -- a denied announcement never happened, so
 * it must not get a receipt). `prevReceipt` threads chaining from
 * whatever manufacturing-chain step preceded this one in the real session
 * (e.g. the sandbox-execution/test-result receipt that triggered this
 * announcement).
 */
export function buildAnnouncement(
  severity: AnnouncementSeverity,
  text: string,
  activeMode: PolicyId = DEFAULT_ACTIVE_MODE,
  prevReceipt?: TransitionReceipt,
): Announcement {
  const decision = checkPolicy(`accessibility_announce_${severity}`, activeMode);
  if (!decision.allowed) {
    throw new Error(`accessibility-platform-adapter refused: ${decision.reason ?? "policy denied"}`);
  }
  const receipt = emitReceipt("accessibility-projection", {
    used: [severity],
    label: `accessibility-projection: ${severity}`,
    generated: text,
    timestamp: Date.now(),
    prevReceipt,
  });
  return { text, ariaLive: ariaLiveForSeverity(severity), receipt };
}

/**
 * REQUIRES A REAL BROWSER (Playwright, not unit-testable here): writes the
 * announcement text into a real `[aria-live]` DOM node so screen readers
 * pick it up. Left as a thin, honestly-labeled function rather than
 * mocking `document` -- exercised only in tests/harness/playwright-setup.ts
 * consumers.
 */
export function announceToLiveRegion(doc: Document, regionId: string, announcement: Announcement): void {
  const region = doc.getElementById(regionId);
  if (!region) throw new Error(`accessibility-platform-adapter: no live region #${regionId} in the DOM`);
  region.setAttribute("aria-live", announcement.ariaLive);
  region.textContent = announcement.text;
}

/**
 * REQUIRES A REAL BROWSER (Playwright, not unit-testable here): speaks
 * `text` via the real Web Speech API. `window.speechSynthesis` has no
 * jsdom polyfill.
 */
export function speak(win: Window, text: string): void {
  const synth = (win as unknown as { speechSynthesis?: SpeechSynthesis }).speechSynthesis;
  if (!synth) throw new Error("accessibility-platform-adapter: speechSynthesis unavailable in this environment");
  const utterance = new (win as unknown as { SpeechSynthesisUtterance: typeof SpeechSynthesisUtterance }).SpeechSynthesisUtterance(text);
  synth.speak(utterance);
}

/**
 * Reduction path: if a future W3C standard exposes a more declarative
 * accessibility-preference API (e.g. a standardized `aria-live` policy
 * schema), more of the severity->politeness mapping could become
 * generated configuration data rather than this hand-written function;
 * the real-DOM/real-SpeechSynthesis half remains irreducibly custom by
 * nature (it is, by definition, a live browser platform binding).
 */
export const REDUCTION_PATH_NOTE =
  "ariaLiveForSeverity/buildAnnouncement are DOM-free and unit-tested; " +
  "announceToLiveRegion/speak require a real browser and are exercised " +
  "only via Playwright.";
