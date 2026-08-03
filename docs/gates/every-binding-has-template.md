
# Gate `every-binding-has-template`: Every TemplateBinding Has a Template File


> All cmx:TemplateBinding individuals must declare cmx:templateFile.


| Field | Value |
|-------|-------|
| Gate ID | `every-binding-has-template` |
| Surface | `execution` |
| Invariant | ∀ b ∈ cmx:TemplateBinding → ∃ cmx:templateFile(b) |
| ASK body | see [`.specify/gates/every-binding-has-template.rq`](.specify/gates/every-binding-has-template.rq) |

## Invariant

∀ b ∈ cmx:TemplateBinding → ∃ cmx:templateFile(b)

## ASK Body

```sparql

    NOT EXISTS {
      ?b a cmx:TemplateBinding .
      FILTER NOT EXISTS { ?b cmx:templateFile ?f }
    }
    
```

---
*Generated from `combinatorial.ttl`. Edit the ontology, not this file. Regenerate: `ggen sync`.*
