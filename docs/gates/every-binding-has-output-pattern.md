
# Gate `every-binding-has-output-pattern`: Every TemplateBinding Has an Output Pattern


> All cmx:TemplateBinding individuals must declare cmx:outputPattern.


| Field | Value |
|-------|-------|
| Gate ID | `every-binding-has-output-pattern` |
| Surface | `causality` |
| Invariant | ∀ b ∈ cmx:TemplateBinding → ∃ cmx:outputPattern(b) |
| ASK body | see [`.specify/gates/every-binding-has-output-pattern.rq`](.specify/gates/every-binding-has-output-pattern.rq) |

## Invariant

∀ b ∈ cmx:TemplateBinding → ∃ cmx:outputPattern(b)

## ASK Body

```sparql
PREFIX cmx: <http://ggen.org/combinatorial#>


    ?b a cmx:TemplateBinding .
    FILTER NOT EXISTS { ?b cmx:outputPattern ?p }
    
```

---
*Generated from `combinatorial.ttl`. Edit the ontology, not this file. Regenerate: `ggen sync`.*
