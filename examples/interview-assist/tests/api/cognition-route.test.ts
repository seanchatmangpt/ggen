/**
 * Real Chicago-TDD test for app/api/cognition/route.ts, no mocks: invokes
 * the real Next.js route handler function directly (same module the
 * running `next dev`/`next start` server dispatches this route to) with a
 * real `NextRequest`, which internally calls the real
 * wasm4pm-cognition WASM bridge (cognition-adapter.ts).
 *
 * Production-hardening pass: covers the graceful-degradation path added to
 * this route -- a genuinely broken `x-wasm4pm-cognition-module-path` test
 * header makes `runCognition`'s real `require()` genuinely fail, and the
 * route must turn that into a clean, typed 503 JSON body, never a raw
 * 500/unhandled-exception response. Same pattern already established by
 * tests/api/test-route.test.ts.
 */
import { describe, it, expect } from "vitest";
import { NextRequest } from "next/server";
import { POST } from "../../app/api/cognition/route";

function postRequest(body: unknown, headers: Record<string, string> = {}): NextRequest {
  return new NextRequest("http://localhost:3000/api/cognition", {
    method: "POST",
    headers: { "content-type": "application/json", ...headers },
    body: JSON.stringify(body),
  });
}

describe("app/api/cognition/route.ts POST (real wasm4pm-cognition dispatch, no mocks)", () => {
  it("returns 200 with a real matched outcome for a real ARRAY-keyword intent", async () => {
    const response = await POST(postRequest({ intent: "I have an array of numbers to search through" }));
    expect(response.status).toBe(200);
    const json = (await response.json()) as { status: string; selected?: string };
    expect(json.status).toBe("matched");
    expect(json.selected).toBe("ARRAY");
  });

  it("returns 422 for a real no-track-matched intent", async () => {
    const response = await POST(postRequest({ intent: "hello there, nice weather today" }));
    expect(response.status).toBe(422);
    const json = (await response.json()) as { status: string };
    expect(json.status).toBe("no-track-matched");
  });

  it("returns 400 for a request body with no intent", async () => {
    const response = await POST(postRequest({}));
    expect(response.status).toBe(400);
  });

  describe("graceful WASM-load-failure handling (production-hardening pass)", () => {
    const FORCE_UNAVAILABLE_HEADERS = { "x-wasm4pm-cognition-force-unavailable": "1" };

    it("a genuine forced require() failure (via the test-only header) yields a clean typed 503, not a raw 500", async () => {
      const response = await POST(
        postRequest({ intent: "I have an array of numbers to search through" }, FORCE_UNAVAILABLE_HEADERS),
      );
      expect(response.status).toBe(503);
      const json = (await response.json()) as { status: string; reason?: string };
      expect(json.status).toBe("unavailable");
      expect(typeof json.reason).toBe("string");
      expect(json.reason!.length).toBeGreaterThan(0);
      expect(json.reason).toMatch(/intentionally broken/i);
    });

    it("the real happy path is unaffected -- omitting the header still resolves the real module and matches normally", async () => {
      const response = await POST(postRequest({ intent: "what target value are we aiming for" }));
      expect(response.status).toBe(200);
      const json = (await response.json()) as { status: string; selected?: string };
      expect(json.status).toBe("matched");
      expect(json.selected).toBe("TARGET");
    });

    it("recovers on the very next real request after a forced-unavailable request -- the forced failure never poisons the shared module cache", async () => {
      await POST(postRequest({ intent: "do we need the indices of the matches" }, FORCE_UNAVAILABLE_HEADERS));
      const recovered = await POST(postRequest({ intent: "do we need the indices of the matches" }));
      expect(recovered.status).toBe(200);
      const json = (await recovered.json()) as { status: string; selected?: string };
      expect(json.status).toBe("matched");
      expect(json.selected).toBe("INDICES");
    });
  });
});
