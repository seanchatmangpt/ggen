# Ultra-Patterns Matrix (All Patterns Compressed)

## GENERATE ANYTHING (Pattern Library)

### REST API
```sparql
SELECT ?class ?prop ?type WHERE {?class a rdfs:Class; ex:method ?m. ?class ex:path ?path. ?prop rdfs:domain ?class; rdfs:range ?type}
```
```jinja2
{% for class in classes %}
#[{{ class.method | upper }}("{{ class.path }}")]
pub async fn {{ class.name }}({% for prop in class.properties %}{{ prop.name }}: {{ prop.range }}{% endfor %}) -> Result<Response> {}
{% endfor %}
```

### GraphQL
```sparql
SELECT ?type ?field ?scalarType WHERE {?type ex:graphqlType true. ?field rdfs:domain ?type; rdfs:range ?scalarType}
```
```jinja2
type {{ class.name }} {
  {% for field in class.properties %}
  {{ field.name }}: {{ field.range }}{% if field.required %}!{% endif %}
  {% endfor %}
}
```

### gRPC (.proto)
```sparql
SELECT ?service ?method ?request ?response WHERE {?service ex:grpcService true. ?method ex:rpcMethod ?rpc. ?rpc ex:request ?request; ex:response ?response}
```
```jinja2
service {{ service.name }} {
  {% for method in service.methods %}
  rpc {{ method.name }}({{ method.request }}) returns ({{ method.response }});
  {% endfor %}
}
message {{ class.name }} {
  {% for field in class.properties %}
  {{ field.range }} {{ field.name }} = {{ loop.index }};
  {% endfor %}
}
```

### Database Schema
```sparql
SELECT ?table ?column ?type WHERE {?table ex:sqlTable true. ?column rdfs:domain ?table; rdfs:range ?type}
```
```jinja2
CREATE TABLE {{ table.name }} (
  {% for col in table.columns %}
  {{ col.name }} {{ col.type }}{% if col.primaryKey %} PRIMARY KEY{% endif %}{% if col.required %} NOT NULL{% endif %},
  {% endfor %}
);
```

### Validation
```sparql
SELECT ?prop ?minLen ?maxLen ?pattern ?min ?max WHERE {?shape sh:property [sh:path ?prop; sh:minLength ?minLen; sh:maxLength ?maxLen; sh:pattern ?pattern; sh:minInclusive ?min; sh:maxInclusive ?max]}
```
```jinja2
impl Validate for {{ class.name }} {
  fn validate(&self) -> Result<()> {
    {% for prop in class.properties %}
    if self.{{ prop.name }}.len() < {{ prop.minLen }} { return Err(...); }
    if self.{{ prop.name }}.len() > {{ prop.maxLen }} { return Err(...); }
    if !regex::Regex::new("{{ prop.pattern }}")?.is_match(&self.{{ prop.name }}) { return Err(...); }
    {% endfor %}
    Ok(())
  }
}
```

### Tests (Scenarios)
```sparql
SELECT ?entity ?scenario ?expected ?input ?output WHERE {?scenario test:forEntity ?entity; test:givenInput ?input; test:expectOutput ?expected}
```
```jinja2
{% for scenario in scenarios %}
#[test]
fn test_{{ scenario.name }}() {
  let input = {{ scenario.input }};
  let result = process(input);
  assert_eq!(result, {{ scenario.expected }});
}
{% endfor %}
```

### Permissions
```sparql
SELECT ?role ?resource ?action WHERE {?role ex:role true. ?permission :grants [ex:resource ?resource; ex:action ?action]}
```
```jinja2
impl Authorize for {{ role.name }} {
  fn can_{{ action }}(&self, resource: &{{ resource }}) -> bool {
    {% if role.allActions %}true{% else %}false{% endif %}
  }
}
```

### State Machines
```sparql
SELECT ?state ?nextState ?condition WHERE {?state ex:state true; ex:transition [ex:to ?nextState; ex:when ?condition]}
```
```jinja2
#[derive(Clone, Copy)]
pub enum State {
  {% for state in states %}{{ state.name }},{% endfor %}
}
impl State {
  pub fn transition(&self, event: &Event) -> Option<State> {
    match self {
      {% for state in states %}
      State::{{ state.name }} => match event {
        {% for trans in state.transitions %}
        Event::{{ trans.on }} if {{ trans.condition }} => Some(State::{{ trans.to }}),
        {% endfor %}
        _ => None,
      },
      {% endfor %}
    }
  }
}
```

### Migrations
```sparql
SELECT ?version ?script ?up ?down WHERE {?migration ex:version ?version; ex:upScript ?up; ex:downScript ?down}
```
```jinja2
pub async fn migrate(conn: &PgPool) -> Result<()> {
  {% for migration in migrations %}
  sqlx::query("{{ migration.up }}").execute(conn).await?;
  {% endfor %}
  Ok(())
}
```

### Kafka Schemas
```sparql
SELECT ?topic ?key ?value WHERE {?topic ex:kafkaTopic true; ex:keySchema ?key; ex:valueSchema ?value}
```
```jinja2
{% for topic in topics %}
pub struct {{ topic.key }} {
  {% for field in topic.keyFields %}
  pub {{ field.name }}: {{ field.type }},
  {% endfor %}
}
pub struct {{ topic.value }} {
  {% for field in topic.valueFields %}
  pub {{ field.name }}: {{ field.type }},
  {% endfor %}
}
{% endfor %}
```

---

## INFERENCE RULES (Library)

| Rule | SPARQL | Materializes |
|------|--------|--------------|
| **Hierarchy** | `?s rdfs:subClassOf+ ?o` | `:allSuper`, `:allSub` |
| **Inherit Props** | Domain expand | `:domain` on children |
| **Inherit Caps** | Capability aggregate | `:canCreate/:canRead/etc` |
| **Validation** | SHACL extract | `:minLen`, `:pattern` |
| **Traits** | Mark by superclass | `:isAuditable`, `:isSoft` |
| **Unused** | No instances | `:unused` |
| **Duplicates** | Aggregate by property | `:isDuplicate`, count |
| **Cycles** | Transitive closure | `:circular` |
| **Denormalize** | Flatten hierarchy | `:flatProp` |
| **Compute** | Derived facts | `:derived` |

---

## QUICK TEMPLATES

### Minimal Config
```toml
[project]
name = "p"
[generation]
ontology_dir = "o/"
templates_dir = "t/"
output_dir = "g/"
[[inference.rules]]
name = "h"
query = "CONSTRUCT {?s :all ?o} WHERE {?s rdfs:subClassOf+ ?o}"
[[generation.rules]]
name = "r"
template = "t.tera"
output_file = "o.rs"
```

### Minimal Ontology
```turtle
@prefix ex: <https://example.com/>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
ex:A a rdfs:Class.
ex:B a rdfs:Class; rdfs:subClassOf ex:A.
ex:p a rdf:Property; rdfs:domain ex:A; rdfs:range xsd:string.
```

### Minimal Template
```jinja2
{% for class in classes %}pub struct {{ class.name }} {}{% endfor %}
```

---

## MULTI-LANGUAGE SNIPPETS

| Language | Pattern |
|----------|---------|
| **Rust** | `pub struct {{ name }} { {% for p in props %}pub {{ p }}: Type,{% endfor %} }` |
| **Python** | `class {{ name }}:` + `    self.{{ p }}: Type` |
| **JS** | `class {{ name }} { constructor({% for p in props %}{{ p }},{% endfor %}) {} }` |
| **Go** | `type {{ name }} struct { {% for p in props %}{{ p | upper }} string{% endfor %} }` |
| **Java** | `public class {{ name }} { {% for p in props %}private Type {{ p }};{% endfor %} }` |

---

## INTEGRATION QUICK REFS

| Stack | Setup |
|-------|-------|
| **Rust+Postgres** | sqlx + migrations from RDF schema |
| **Python+FastAPI** | Pydantic models from RDF classes |
| **Node+Express** | OpenAPI from RDF properties |
| **Go+gRPC** | .proto generation from RDF |
| **GraphQL+Apollo** | Type defs from RDF classes |
| **Kafka** | Avro schemas from RDF topics |

---

## TROUBLESHOOT

| Symptom | Cause | Fix |
|---------|-------|-----|
| No output | Missing template | Check `templates_dir` exists |
| Parse error | Bad TTL | `rapper -c file.ttl` |
| Validation fail | Bad ontology | Run `--validate-only -vvv` |
| Slow | Large ontology | Add LIMIT to SPARQL |
| Timeout | Hanging query | Use `--timeout` flag |
| Files not written | Protected paths | Use `--force` or change `protected_paths` |
| Wrong output | Template bug | Check syntax with `--dry-run` |

---

## ENV VARS

```bash
GGEN_CONFIG=alt.toml
GGEN_LOGLEVEL=debug
RUST_LOG=ggen=debug
GGEN_HOME=~/.cache/ggen
GGEN_TIMEOUT=120000
```

---

## ONE-LINERS

```bash
# Validate
ggen sync --validate-only --format json

# Check what would happen
ggen sync --dry-run -vvv

# Single rule
ggen sync --rule inheritance_closure

# Live reload
ggen sync --watch

# Production
ggen sync --config prod.toml --audit audit.json

# CI/CD
ggen sync --validate-only && cargo test

# Debug
ggen sync --timeout 120000 -vvv --audit debug.json

# Multi-project
for proj in crates/*; do ggen sync --config $proj/ggen.toml; done
```

