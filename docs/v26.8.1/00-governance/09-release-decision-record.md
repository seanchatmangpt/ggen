# Release decision record

## Decision under research

v26.8.1 is intended to establish a closed, evidence-backed software manufacturing system and to determine whether ggen-legacy may be sunset without information loss.

## Present standing

`PARTIAL_ALIVE` for corpus creation. The repository has strong evidence primitives—deterministic graph projection, BLAKE3 receipts, replay, self-hosting, Gall checkpoints, executable guards, and multiple production surfaces—but this PR does not claim the integrated v26.8.1 crown.

## Crown decision rule

Release requires one exact aggregate source head with:

- complete subsystem coverage;
- integrated end-to-end execution;
- real verifier evidence across declared suites;
- benchmark receipts separating graph, render, write, compile, test, and receipt stages;
- zero unresolved authority conflicts;
- zero unclassified legacy capabilities;
- external witness promotion.

The final decision must be machine-readable and must refuse release or sunset when any required conjunct is missing.
