
# Gate `every-generator-has-action`: Every Generator Has At Least One Action


> All declared cmx:Generator individuals must have at least one cmx:hasAction link.


| Field | Value |
|-------|-------|
| Gate ID | `every-generator-has-action` |
| Surface | `state` |
| Invariant | ∀ gen ∈ cmx:Generator → ∃ cmx:hasAction(gen, act) |
| ASK body | see [`.specify/gates/every-generator-has-action.rq`](.specify/gates/every-generator-has-action.rq) |

## Invariant

∀ gen ∈ cmx:Generator → ∃ cmx:hasAction(gen, act)

## ASK Body

```sparql
PREFIX cmx: <http://ggen.org/combinatorial#>


    ?gen a cmx:Generator .
    FILTER NOT EXISTS { ?gen cmx:hasAction ?act }
    
```

---
*Generated from `combinatorial.ttl`. Edit the ontology, not this file. Regenerate: `ggen sync`.*
