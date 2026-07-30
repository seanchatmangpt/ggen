# 5. TOGAF Reframed as an Executable Method

TOGAF provides a disciplined method for developing and governing enterprise architecture. Its enduring value is not a prescribed set of diagrams. It is the insistence that architecture change move through explicit concerns: preparation, vision, business, information systems, technology, solution selection, migration, governance, and change management, with requirements present throughout.

ggen reframes each phase as a graph transformation.

A phase begins with an admitted architecture state and a set of requirements. It produces candidate facts, decisions, work products, and obligations. Those outputs are not promoted automatically. They pass validation and receive standing before becoming inputs to later phases.

```text
ArchitectureState_n
+ Requirements
+ PhaseOperator
-> CandidateState
-> Validation
-> PromotionReceipt
-> ArchitectureState_n+1
```

This interpretation solves two problems.

First, it preserves iteration. TOGAF is often implemented as a document sequence even though architecture development is inherently iterative. Graph transformations can revisit earlier assumptions without losing lineage.

Second, it makes requirements operational. A requirement is not merely referenced in a matrix. It can constrain candidate solutions, generate tests, affect plans, and appear in compliance receipts.

The ADM phases remain distinct because they answer different questions:

- Preliminary: what architecture capability and governance exist?
- A: what outcome and scope are authorized?
- B: what operating model and business capabilities are required?
- C: what information and application architecture realize them?
- D: what technology and operating envelope support them?
- E: which solution combinations are viable?
- F: how will the enterprise transition?
- G: does implementation conform?
- H: how will observed change alter the architecture?

The executable interpretation must not erase human decision. Architecture vision, risk acceptance, organizational design, and exception approval may remain human authorities. ggen provides the evidence and consequence surface on which those decisions operate.

The method becomes more rigorous because it can distinguish a missing decision from a failed computation. UNKNOWN, UNSUPPORTED, REFUSED, and PARTIAL_ALIVE remain visible rather than being flattened into a green roadmap status.
