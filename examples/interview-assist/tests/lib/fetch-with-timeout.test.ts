/**
 * Real Chicago-TDD test for lib/client/fetch-with-timeout.ts, no mocks: a
 * real local `node:http` server (real socket, real network round trip on
 * 127.0.0.1) and a real `AbortController`-backed `fetch()` -- not a faked
 * timer or a stubbed fetch implementation. Node 22's built-in `fetch`
 * (this repo's `engines.node` requires >=22) is used directly, the same
 * global `fetchWithTimeout` itself calls.
 */
import { describe, it, expect, afterEach } from "vitest";
import http from "node:http";
import type { AddressInfo } from "node:net";
import { fetchWithTimeout, RequestTimeoutError, describeFetchError } from "../../lib/client/fetch-with-timeout";

describe("fetchWithTimeout (real AbortController + real local HTTP server, no mocks)", () => {
  let server: http.Server | undefined;

  async function startSlowServer(delayMs: number): Promise<string> {
    server = http.createServer((_req, res) => {
      setTimeout(() => {
        res.writeHead(200, { "content-type": "application/json" });
        res.end(JSON.stringify({ ok: true }));
      }, delayMs);
    });
    await new Promise<void>((resolve) => server!.listen(0, "127.0.0.1", resolve));
    const address = server.address() as AddressInfo;
    return `http://127.0.0.1:${address.port}`;
  }

  afterEach(async () => {
    if (server) {
      await new Promise<void>((resolve) => server!.close(() => resolve()));
      server = undefined;
    }
  });

  it("genuinely aborts a real, slow HTTP request once the configured timeout elapses, throwing RequestTimeoutError", async () => {
    const url = await startSlowServer(2_000);
    const start = Date.now();
    await expect(fetchWithTimeout(url, {}, 200)).rejects.toBeInstanceOf(RequestTimeoutError);
    const elapsed = Date.now() - start;
    // Real evidence the abort fired near the configured 200ms budget, not
    // after the full real 2000ms server-side delay -- proves the request
    // was actually cancelled, not merely that the promise eventually
    // settled some other way.
    expect(elapsed).toBeLessThan(1_000);
  });

  it("resolves normally, returning the real response, for a real fast request well under the timeout budget", async () => {
    const url = await startSlowServer(10);
    const res = await fetchWithTimeout(url, {}, 2_000);
    expect(res.status).toBe(200);
    const json = (await res.json()) as { ok: boolean };
    expect(json).toEqual({ ok: true });
  });

  it("rejects with the real underlying error (not RequestTimeoutError) for a genuine connection failure unrelated to timing", async () => {
    // Nothing is listening on this real, momentarily-unused local port --
    // a genuine ECONNREFUSED, not a timeout.
    await expect(fetchWithTimeout("http://127.0.0.1:1", {}, 5_000)).rejects.not.toBeInstanceOf(RequestTimeoutError);
  });

  it("describeFetchError gives an honest, distinct message for a real timeout vs a real generic failure", () => {
    const timeoutMessage = describeFetchError(new RequestTimeoutError("http://example.invalid", 100));
    expect(timeoutMessage).toMatch(/longer than expected/i);
    const genericMessage = describeFetchError(new Error("ECONNREFUSED"));
    expect(genericMessage).toContain("ECONNREFUSED");
  });
});
