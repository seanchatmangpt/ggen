/**
 * UX-polish pass (production-grade loading states): a minimal decorative
 * busy indicator. Paired everywhere it's used with a real `aria-busy`
 * attribute on the control it sits inside, driven by a real in-flight
 * fetch()'s own pending boolean state (see app/page.tsx's
 * running/runningTest/cognitionSubmitting/finishing state and
 * components/session-menu.tsx's `finishing` prop) -- this component itself
 * is purely the VISUAL half.
 *
 * `aria-hidden` deliberately: every caller also changes the host control's
 * own accessible name/text to a "...ing" label (e.g. "Submitting...",
 * "Running..."), which is what screen readers actually announce for the
 * pending state. A redundant `role="status"`/live region on the spinner
 * itself would double-announce the same fact through two different
 * channels.
 *
 * Respects the app's real reduced-motion setting for free: app/globals.css
 * already has a blanket `:root[data-reduced-motion="true"] *` rule
 * (JTBD 6 closure) that forces every animation's duration to ~0 -- this is
 * just another animated element, no special-casing needed.
 */
export interface SpinnerProps {
  "data-testid"?: string;
}

export function Spinner({ "data-testid": testId }: SpinnerProps) {
  return <span className="spinner" aria-hidden="true" data-testid={testId} />;
}
