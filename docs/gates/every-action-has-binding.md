
# Gate `every-action-has-binding`: Every Action Has a TemplateBinding


> All cmx:Action individuals must have exactly one cmx:hasBinding link to a cmx:TemplateBinding.


| Field | Value |
|-------|-------|
| Gate ID | `every-action-has-binding` |
| Surface | `state` |
| Invariant | ∀ act ∈ cmx:Action → ∃ cmx:hasBinding(act, b) |
| ASK body | see [`.specify/gates/every-action-has-binding.rq`](.specify/gates/every-action-has-binding.rq) |

## Invariant

∀ act ∈ cmx:Action → ∃ cmx:hasBinding(act, b)

## ASK Body

```sparql
PREFIX cmx: <http://ggen.org/combinatorial#>


    ?act a cmx:Action .
    FILTER NOT EXISTS { ?act cmx:hasBinding ?b }
    
```

---
*Generated from `combinatorial.ttl`. Edit the ontology, not this file. Regenerate: `ggen sync`.*
