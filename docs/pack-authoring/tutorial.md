# Tutorial: build your first ggen pack

This walks through creating one working pack from nothing, end to end. By
the finish you will have run `ggen sync` and watched a real file land on
disk from an RDF fact.

## 1. Lay out the directory

```
packs/hello-pack/
  pack.toml
  ontology.ttl
  templates/
    greeting.txt.tmpl
```

## 2. `pack.toml`

```toml
[pack]
name = "hello-pack"
version = "0.1.0"
description = "Says hello, from RDF."
```

## 3. `ontology.ttl` — one class, one individual

```turtle
@prefix hp: <http://example.org/hello-pack#> .

hp:Greeting a hp:GreetingClass ;
    hp:text "Hello from ggen." .
```

## 4. `templates/greeting.txt.tmpl` — one template

```
---
to: "output/greeting.txt"
sparql:
  row: |
    PREFIX hp: <http://example.org/hello-pack#>
    SELECT ?text WHERE { hp:Greeting hp:text ?text . } ORDER BY ?text
---
{{ row[0].text }}
```

Templates never synthesize content at render time — they echo a literal
already stored in the ontology. If the string isn't in the RDF, it can't
appear in the output. This is the same "verbatim, not synthesized" contract
`gh-terraform-pack` uses for every one of its 40+ templates.

## 5. Wire a consumer project and sync

```toml
# consumer/ggen.toml
[project]
name = "hello-consumer"

[ontology]
source = "ontology.ttl"

[packs]
hello-pack = { path = "../hello-pack" }

[templates]
dir = "templates"
```

```bash
ggen sync run
cat output/greeting.txt   # "Hello from ggen."
```

Run it again — nothing changes. That's `ggen`'s idempotency guarantee, and
it's the first thing your pack's Chicago-TDD test should prove (see the
how-to below).

## Next

- **`how-to-add-a-gate.md`** — refuse bad facts before they generate bad files.
- **`how-to-write-a-chicago-e2e-test.md`** — prove your pack works, for real.
- **`reference.md`** — the full file-layout contract.
- **`explanation.md`** — why packs are shaped this way.
