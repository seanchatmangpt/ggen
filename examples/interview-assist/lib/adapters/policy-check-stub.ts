/**
 * PENDING GENERATED PORT — TICKET-028 (precondition/policy/target-authority
 * check projection) does not exist on disk yet in this workstream's build
 * order (workstream H ran before workstream F's generation in this session).
 *
 * Every adapter in this directory is required (by TICKET-034..039's shared
 * "Domain-data responsibility: NONE" / "no bypass of TICKET-028's checks"
 * clause) to call a policy/precondition check BEFORE performing its real
 * action, never after. Since the real generated check is not available yet,
 * this file is an explicitly-labeled placeholder with the exact shape
 * TICKET-028 is expected to produce (a synchronous predicate over a
 * capability id). It default-allows (returns `{ allowed: true }`) — it does
 * NOT invent domain policy logic itself (no capability allow/deny table is
 * hardcoded here beyond "unconditionally defer to the real check once it
 * exists").
 *
 * Replace `checkPolicy`'s body with a real import from TICKET-028's
 * generated module the moment it lands; every adapter in this directory
 * already calls through this indirection, so that swap is a one-line change
 * per adapter (just the import), not a re-plumb.
 */

export interface PolicyCheckResult {
  allowed: boolean;
  reason?: string;
}

export interface PolicyCheckRequest {
  capability: string;
}

/**
 * PENDING(TICKET-028): default-allow placeholder. Does not encode any
 * domain rule; this is intentionally NOT a reimplementation of policy
 * logic, only a call-site placeholder that always defers.
 */
export function checkPolicy(request: PolicyCheckRequest): PolicyCheckResult {
  void request;
  return { allowed: true, reason: "PENDING(TICKET-028): no generated policy check wired yet; default-allow placeholder" };
}
