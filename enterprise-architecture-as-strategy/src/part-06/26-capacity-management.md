# 26. Capacity Management as Architecture

Capacity management should begin before runtime.

Every ontology, pack, planner, projection, and product should publish a measured operating envelope. The envelope is contextual: hardware, runtime, dataset, cache state, and configuration affect results.

A capacity receipt should include:

- workload fingerprint;
- environment fingerprint;
- phase timings;
- memory;
- throughput;
- p50, p95, and p99;
- growth slope;
- first warning;
- first refusal;
- baseline comparison;
- confidence.

Stress suites should vary dimensions independently and in combination. Ontology tests vary document count, quads, import depth, blank nodes, rules, validation, and template count. Pack tests vary output count, output size, query complexity, consumers, and parallelism. Planner tests vary state size, action count, branching, temporal constraints, and resource contention.

The system should detect knees rather than rely solely on fixed thresholds. A nonlinear increase in slope can justify a warning before the absolute latency budget is crossed.

Warnings must explain remediation:

- use a smaller ontology profile;
- enable parse or graph cache;
- defer materialization;
- partition a pack;
- reduce validation scope;
- move work to a cold path;
- precompile query plans;
- change environment class.

A capacity warning is an architecture decision aid, not a failure. A refusal occurs only when a declared safety or service budget would be violated.

The capacity repository becomes a learning system. Production receipts update estimates. The architecture can then compare predicted and observed cost and recalibrate future plans.

This is a direct example of architecture becoming operational strategy: platform choices, product promises, and migration sequencing are grounded in measured envelopes rather than aesthetic preference.
