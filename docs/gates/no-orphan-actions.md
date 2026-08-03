
# Gate `no-orphan-actions`: No Orphan Actions


> Every cmx:Action must be reachable from exactly one cmx:Generator via cmx:hasAction.


| Field | Value |
|-------|-------|
| Gate ID | `no-orphan-actions` |
| Surface | `causality` |
| Invariant | ∀ act ∈ cmx:Action → ∃ gen: cmx:hasAction(gen, act) |
| ASK body | see [`.specify/gates/no-orphan-actions.rq`](.specify/gates/no-orphan-actions.rq) |

## Invariant

∀ act ∈ cmx:Action → ∃ gen: cmx:hasAction(gen, act)

## ASK Body

```sparql
PREFIX cmx: <http://ggen.org/combinatorial#>


    ?act a cmx:Action .
    {
      FILTER NOT EXISTS { ?gen cmx:hasAction ?act }
    } UNION {
      ?gen1 cmx:hasAction ?act .
      ?gen2 cmx:hasAction ?act .
      FILTER(?gen1 != ?gen2)
    }
    
```

---
*Generated from `combinatorial.ttl`. Edit the ontology, not this file. Regenerate: `ggen sync`.*
