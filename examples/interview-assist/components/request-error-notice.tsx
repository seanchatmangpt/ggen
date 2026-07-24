/**
 * UX-polish pass (timeout/retry): a small, shared presentation for "a real
 * network request failed or timed out" plus a real Retry affordance.
 * Deliberately dumb, same discipline as cognition-panel.tsx/session-menu.tsx
 * -- owns no fetch call, no timer; `onRetry` is the caller's own real async
 * action function (re-invoking it performs a brand-new real request, never
 * a replay of cached/fabricated data).
 *
 * `role="alert"` so assistive tech announces the failure without the page
 * needing to move focus -- same pattern already used by
 * refusal-presentation.tsx and cognition-panel.tsx's `refused` branch for a
 * comparable "something went wrong, here's why" moment.
 */
export interface RequestErrorNoticeProps {
  message: string;
  onRetry: () => void;
  "data-testid": string;
}

export function RequestErrorNotice({ message, onRetry, "data-testid": testId }: RequestErrorNoticeProps) {
  return (
    <p role="alert" className="request-error-notice" data-testid={testId}>
      <span data-testid={`${testId}-message`}>{message}</span>{" "}
      <button type="button" onClick={onRetry} data-testid={`${testId}-retry`}>
        Retry
      </button>
    </p>
  );
}
