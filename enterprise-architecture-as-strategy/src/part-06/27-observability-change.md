# 27. Observability and Architecture Change Management

An architecture claim should generate the means required to observe it.

If a service declares a dependency on a queue with at-least-once delivery, the architecture may entail metrics for lag, duplicate processing, dead-letter depth, idempotency failures, and replay count.

If a capability declares critical availability, the architecture may entail multi-zone checks, failover scenarios, recovery-time metrics, and incident escalation.

Observability projections include:

- OpenTelemetry semantic conventions;
- metrics;
- traces;
- logs;
- dashboards;
- alerts;
- SLOs;
- error budgets;
- incident routes;
- OCEL mappings;
- conformance rules.

Runtime events return to the architecture graph as observations. They do not overwrite architecture directly. They pass admission and may trigger change proposals.

Change triggers include:

- repeated policy violations;
- capacity regression;
- new consumer pattern;
- failed assumption;
- dependency drift;
- unowned asset;
- deprecated version usage;
- recurring exception;
- cost anomaly;
- retirement readiness.

The architecture change process should produce a bounded delta:

```text
observed deviation
-> diagnosis
-> candidate architecture change
-> impact analysis
-> transition plan
-> governance
```

This prevents local incident repair from silently becoming permanent architecture.

The operating architecture must also detect false confidence. Missing telemetry should produce UNKNOWN, not conformance. A successful request path does not prove resilience. A dashboard with no claim relationship is decoration.

Observability is complete when every strategic architecture claim has a corresponding evidence path and every evidence path can identify the claim it informs.
