# 15. mfact, procint, and Formal Assurance

Formal assurance provides theorem-level standing for claims that can be expressed within a mathematical model.

mfact and procint use Lean to encode definitions, invariants, transition systems, and proofs. This supports claims such as boundedness, determinism, monotonicity, correspondence between abstract operators, and impossibility of certain invalid states.

Formal proof is powerful because it removes entire classes of test uncertainty. It is also easy to overclaim.

A theorem establishes:

$$
Model \models Property
$$

It does not automatically establish:

$$
DeployedSystem \models Property
$$

The second statement requires correspondence evidence. The implementation must be shown to realize the model, the compiler and runtime assumptions must be identified, and the operational environment must remain within the proved boundary.

The architecture graph should therefore distinguish:

- theorem claim;
- formal model;
- kernel-checked proof;
- implementation correspondence;
- build evidence;
- runtime observation;
- residual assumptions.

A formal rail may be ALIVE while correspondence is PARTIAL_ALIVE.

ggen can strengthen formal assurance by manufacturing proof obligations beside implementation. The same ontology can generate:

- Lean declarations;
- theorem skeletons;
- executable tests;
- negative fixtures;
- model-checker inputs;
- documentation of assumptions;
- receipt schemas.

The independent proof burden prevents the generated code from certifying itself through tautological tests.

Formal assurance is most strategic when applied to constitutional boundaries: no unreceipted actuation, authority monotonicity, deterministic projection, closure of external pack dependencies, and well-founded workflow descent.

These theorems define what the ecosystem refuses to become, not merely what it can produce.
