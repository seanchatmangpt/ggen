# 4. The Enterprise Architecture Operating System

The ggen ecosystem can be understood as an enterprise architecture operating system.

An operating system does not decide the purpose of every program. It provides the abstractions, isolation, scheduling, storage, interfaces, and control boundaries through which programs operate. In the same way, the architecture operating system does not decide every business choice. It provides the governed mechanisms through which choices become implementable and observable.

Its kernel concepts are:

- identity;
- admission;
- capability;
- dependency;
- authority;
- transition;
- artifact;
- evidence;
- standing;
- lifecycle.

Its services are:

- architecture repository;
- ontology registry;
- pack registry;
- requirements management;
- option exploration;
- transition planning;
- artifact projection;
- compliance evaluation;
- bounded actuation;
- process observation;
- receipt and replay;
- change management.

Its user interfaces include CLI, LSP, APIs, reports, diagrams, dashboards, and generated repositories.

Its scheduler is the transition plan. Its security model is capability-bounded authority. Its filesystem is the architecture repository and artifact graph. Its process table is the set of active plans, packs, products, and execution grants. Its audit log is the receipt and OCEL event graph.

The operating-system analogy also reveals failure modes. A repository that stores architecture objects without lifecycle is a filesystem with no deletion semantics. A pack with hidden dependencies resembles a process with undeclared shared memory. A planner that actuates directly violates privilege separation. A receipt generated before observation is a forged log entry. A universal ontology with unlimited authority is a kernel-space monolith.

The target architecture therefore favors small, explicit mechanisms connected through typed contracts.

The final test of the operating system is not whether every architecture object can be represented. It is whether the system can answer, before a consequential change:

- what is affected;
- what is required;
- what can be generated;
- what must be decided;
- what may execute;
- what evidence will establish success;
- what transition reverses the change;
- what lifecycle state follows.

When those answers are available from the same admitted graph, enterprise architecture has become an operating capability rather than an advisory function.
