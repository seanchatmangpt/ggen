# 25. Governance, Gall, and Standing

Enterprise architecture governance must distinguish authority from evidence.

Constitutional authority defines principles and invariants. Decision authority approves priorities, exceptions, risk, and organizational choices. Evidentiary authority determines what has been demonstrated.

Gall provides the evidentiary mechanism.

A claim receives standing through:

```text
claim
+ positive witness
+ negative fixture
+ adversarial falsifier
+ verifier
+ receipt
+ replay
-> standing
```

The architecture board can then make decisions on a truthful evidence surface.

Gall checkpoints should be architecture transitions, not tickets. Each checkpoint must create a useful bounded system. It should have explicit dependency closure, acceptance conditions, refusal conditions, and a claim ceiling.

An architecture compliance review becomes a generated packet containing:

- contract requirements;
- implementation evidence;
- test and proof outcomes;
- capacity results;
- policy exceptions;
- unresolved risks;
- standing summary;
- promotion recommendation.

The system must preserve partial truth. A capability may pass semantic validation while performance remains UNKNOWN. A deployment may be ALIVE while disaster recovery remains PARTIAL_ALIVE. The summary must not collapse these dimensions.

Exceptions also require lifecycle. An exception should identify owner, scope, rationale, compensating controls, expiration, review trigger, and affected claims. Expired exceptions should produce refusals or renewed decisions.

Governance becomes faster because routine evidence is manufactured and slower only where judgment is actually required.
