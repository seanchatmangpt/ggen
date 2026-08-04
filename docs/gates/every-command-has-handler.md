
# Gate `every-command-has-handler`: Every Command Has Handler


> All declared cli:Command individuals must have a cli:handler value.


| Field | Value |
|-------|-------|
| Gate ID | `every-command-has-handler` |
| Surface | `state` |
| Invariant | ∀ cmd ∈ cli:Command → ∃ cli:handler(cmd) |
| ASK body | see [`.specify/gates/every-command-has-handler.rq`](.specify/gates/every-command-has-handler.rq) |

## Invariant

∀ cmd ∈ cli:Command → ∃ cli:handler(cmd)

## ASK Body

```sparql
PREFIX cli: <http://ggen.org/cli#>


    ?cmd a cli:Command .
    FILTER NOT EXISTS { ?cmd cli:handler ?h }
    
```

---
*Generated from `combinatorial.ttl`. Edit the ontology, not this file. Regenerate: `ggen sync`.*
