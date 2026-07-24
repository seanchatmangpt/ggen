/**
 * UX-polish pass (timeout/retry): a real client-side timeout wrapped around
 * the browser's native `fetch()`, using a real `AbortController` -- not a
 * fabricated timer that merely swaps UI state while the underlying request
 * keeps running. When the timeout fires, the in-flight HTTP request is
 * genuinely aborted (verifiable in a real browser's network panel: the
 * request shows status "(cancelled)"), so the app never has a runaway
 * request left silently in flight after the UI has moved on.
 *
 * Deliberately simple, per this task's own "keep it simple" instruction:
 * one timeout, one typed error class callers can catch, no exponential
 * backoff/jitter/circuit-breaker machinery. Retry is left entirely to the
 * caller -- re-invoking the same async action function is a real retry (a
 * brand-new real request), not a replay of cached data.
 *
 * Client-only (imported only from "use client" app/page.tsx): `fetch` and
 * `AbortController` are both real browser globals here, not a server-side
 * concern -- every server-side call in this app (sandbox-executor.ts,
 * cognition-adapter.ts) already has its own real timeout/refusal path
 * (`ExecutionRefusal.kind === "timeout"`), which this module does not
 * duplicate.
 */

export class RequestTimeoutError extends Error {
  constructor(url: string, timeoutMs: number) {
    super(`Request to ${url} timed out after ${timeoutMs}ms`);
    this.name = "RequestTimeoutError";
  }
}

export async function fetchWithTimeout(
  input: string,
  init: RequestInit,
  timeoutMs: number,
): Promise<Response> {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  try {
    return await fetch(input, { ...init, signal: controller.signal });
  } catch (err) {
    // A real `fetch()` rejection can mean either "we aborted it" (the
    // timeout above fired) or a genuine network-level failure (DNS,
    // connection refused, etc). Distinguish by checking whether OUR
    // controller is the one that aborted, not just `err.name ===
    // "AbortError"`, which could also fire for an abort this function
    // didn't initiate if a caller ever threads its own signal through
    // `init` in the future.
    if (controller.signal.aborted) {
      throw new RequestTimeoutError(input, timeoutMs);
    }
    throw err;
  } finally {
    clearTimeout(timer);
  }
}

/**
 * Maps a value thrown by `fetchWithTimeout` (or any other real network-level
 * failure -- e.g. the dev server briefly unreachable) to a short, honest,
 * human-readable message for `RequestErrorNotice`. Never used to mask a
 * real typed API error the route itself returned in its JSON body (those
 * are parsed from a normal 2xx/4xx/5xx response by the caller and handled
 * separately, before this function would ever run) -- only for `fetch()`
 * itself throwing.
 */
export function describeFetchError(err: unknown): string {
  if (err instanceof RequestTimeoutError) {
    return "That request is taking longer than expected. It may still be running.";
  }
  if (err instanceof Error) {
    return `Request failed: ${err.message}`;
  }
  return "Request failed for an unknown reason.";
}
