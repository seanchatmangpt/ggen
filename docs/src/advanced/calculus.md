<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen calculus (v1)](#ggen-calculus-v1)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen calculus (v1)

State Σ = ⟨T,G,S,C,B,A,M,σ⟩.

Pipeline:
```
project = write ∘ render* ∘ matrix? ∘ bind? ∘ shape ∘ load
```

Laws:
- Determinism
- Idempotent write
- Precedence: CLI > SPARQL > defaults
- Matrix ORDER BY required
