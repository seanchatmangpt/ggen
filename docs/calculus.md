# rgen calculus (v1)

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
