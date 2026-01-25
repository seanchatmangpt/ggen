<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Ultra-Compressed Reference (100% Dense)](#ggen-ultra-compressed-reference-100-dense)
  - [SETUP](#setup)
  - [SPARQL PATTERNS (COPY-PASTE)](#sparql-patterns-copy-paste)
    - [CONSTRUCT (Materialization)](#construct-materialization)
  - [ggen.toml (SCHEMA)](#ggentoml-schema)
  - [CLI](#cli)
  - [TEMPLATES (Tera)](#templates-tera)
  - [COMMON PATTERNS](#common-patterns)
  - [INFERENCE RULES (Template)](#inference-rules-template)
  - [PYTHON AGENT LOOP (Fast)](#python-agent-loop-fast)
  - [GIT WORKFLOW](#git-workflow)
  - [DECISION MATRIX](#decision-matrix)
  - [PERFORMANCE](#performance)
  - [ERROR CODES](#error-codes)
  - [ACRONYMS](#acronyms)
  - [QUICK LOOKUP](#quick-lookup)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Ultra-Compressed Reference (100% Dense)

## SETUP

```bash
mkdir -p ontology/ templates/ && cat > ggen.toml << 'EOF'
[project]
name = "project"
[generation]
ontology_dir = "ontology/"
templates_dir = "templates/"
output_dir = "src/generated/"
EOF
cat > ontology/domain.ttl << 'EOF'
@prefix ex: <https://example.com/>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
ex:Entity a rdfs:Class; rdfs:label "Entity".
ex:id a rdf:Property; rdfs:domain ex:Entity; rdfs:range xsd:string.
EOF
ggen sync
```

---

## SPARQL PATTERNS (COPY-PASTE)

| Query | Pattern |
|-------|---------|
| **Find by type** | `?x a rdfs:Class` |
| **Transitive** | `?x rdfs:subClassOf+ ?y` |
| **Optional** | `OPTIONAL {?x prop ?y}` |
| **Union** | `{?x a Type1} UNION {?x a Type2}` |
| **Filter** | `FILTER (?age > 18 && regex(?name,"^A"))` |
| **Aggregate** | `SELECT ?x (COUNT(?y) AS ?cnt) GROUP BY ?x` |
| **Negate** | `MINUS {?x a Deleted}` |
| **Bind** | `BIND (STR(?x) AS ?name)` |

### CONSTRUCT (Materialization)

```sparql
CONSTRUCT {?s :allSuper ?o} WHERE {?s rdfs:subClassOf+ ?o}
CONSTRUCT {?p rdfs:domain ?sub} WHERE {?p rdfs:domain ?super. ?sub rdfs:subClassOf+ ?super}
CONSTRUCT {?c :hasCap ?cap} WHERE {{?c :canCreate ?t. BIND("Create" AS ?cap)} UNION {?c :canRead ?t. BIND("Read" AS ?cap)}}
CONSTRUCT {?p :minLen ?n} WHERE {?s sh:property [sh:path ?p; sh:minLength ?n]}
CONSTRUCT {?c :isAudit true} WHERE {?c rdfs:subClassOf+ ex:Auditable}
```

---

## ggen.toml (SCHEMA)

```toml
[project]
name = "str" | version = "str" | description = "str" | authors = ["str"]

[generation]
ontology_dir = "dir" | templates_dir = "dir" | output_dir = "dir"
protected_paths = ["glob"] | regenerate_paths = ["glob"] | timeout_ms = 30000

[generation.poka_yoke]
warning_headers = bool | gitignore_generated = bool | gitattributes_generated = bool

[[inference.rules]]
name = "str" | query = "SPARQL" | rule_order = ["order"]

[[generation.rules]]
name = "str" | template = "file" | output_file = "file" | ontology = "file" | query = "SPARQL"

[marketplace]
enabled = bool | registry = "str" | fmea_validation = bool | critical_threshold = 300

[codeowners]
enabled = bool | source_dirs = ["dir"] | base_dirs = ["dir"] | output_path = "file"

[lifecycle.phases.pre_generation]
run = "cmd" | on_error = "fail|warn|ignore"

[security]
sign_packages = bool | verify_signatures = bool | allowed_domains = ["domain"]

[performance]
parallel_templates = bool | max_workers = num | query_timeout_ms = 10000
```

---

## CLI

```bash
ggen sync                              # Run with ggen.toml
ggen sync --config alt.toml            # Alt config
ggen sync --dry-run                    # Don't write
ggen sync --validate-only              # Just validate
ggen sync --rule rule-name             # Single rule
ggen sync --timeout 60000              # 60s limit
ggen sync --audit audit.json           # Capture trail
ggen sync --watch                      # Live-reload
ggen sync --format json                # JSON output
ggen sync -vvv                         # Max verbose
ggen sync --ontology-override dir      # Override dirs
ggen sync --output-override dir
ggen sync --force                      # Ignore protected
```

| Flag | Type | Use |
|------|------|-----|
| `--config` | path | Alt config file |
| `--rule` | str | Specific rule only |
| `--timeout` | ms | Execution limit |
| `--format` | json/text | Output format |
| `--audit` | path | Audit trail |
| `--dry-run` | flag | Don't write |
| `--validate-only` | flag | Check only |
| `--watch` | flag | Live reload |
| `-v` / `-vv` / `-vvv` | flag | Verbosity |

**Exit Codes**: 0=ok, 1=err, 2=parse, 3=gen, 4=validation, 5=timeout

---

## TEMPLATES (Tera)

```jinja2
{% for class in classes %}
  {{ class.name }}                      # Basic
  {{ class | upper }}                   # Filter
  {% if class.properties %}...{% endif %} # Conditional
  {% for prop in class.properties %}    # Loop
    {{ prop.name }}: {{ prop.range }}
  {% endfor %}
{% endfor %}

{{ class.allSuperclasses | join(", ") }}  # From inference rule
{% if class.allSuperclasses %}...{% endif %}
```

---

## COMMON PATTERNS

| Need | SPARQL |
|------|--------|
| Hierarchy | `?x rdfs:subClassOf+ ?y` → `:allSuper` |
| Properties on children | Domain expansion → inherited `:domain` |
| Capabilities | Aggregate `:canCreate/:canRead/:canUpdate/:canDelete` → `:hasCap` |
| Validation | Extract SHACL → `:minLen`, `:maxLen`, `:pattern` |
| Traits | Mark `:isAuditable`, `:isVersioned`, `:isSoftDeletable` |
| Unused | `FILTER NOT EXISTS {?instance a ?class}` |
| Duplicates | `SELECT ?p (COUNT(?c) AS ?cnt) GROUP BY ?p HAVING COUNT > 1` |
| Circular deps | `?a dep+ ?b. ?b dep+ ?c. ?c dep+ ?a` |

---

## INFERENCE RULES (Template)

```sparql
# Hierarchy
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
CONSTRUCT {?s :allSuper ?o} WHERE {?s rdfs:subClassOf+ ?o}

# Property expansion
CONSTRUCT {?p rdfs:domain ?s} WHERE {?p rdfs:domain ?sup. ?s rdfs:subClassOf+ ?sup. MINUS {?p rdfs:domain ?s}}

# Capabilities
PREFIX ex: <https://example.com/>
CONSTRUCT {?c :canCreate ?t} WHERE {?sup :canCreate ?t. ?c rdfs:subClassOf+ ?sup}

# Validation (SHACL)
PREFIX sh: <http://www.w3.org/ns/shacl#>
CONSTRUCT {?p :minLen ?n} WHERE {?shape sh:property [sh:path ?p; sh:minLength ?n]}

# Mark concerns
CONSTRUCT {?c :isAudit true} WHERE {?c rdfs:subClassOf+ ex:Auditable}

# Find unused
CONSTRUCT {?c :unused true} WHERE {?c a rdfs:Class. FILTER NOT EXISTS {?i a ?c. ?c rdfs:subClassOf ?s}}

# Deduplicate (idempotent)
CONSTRUCT {?c :processed true} WHERE {?c a rdfs:Class. MINUS {?c :processed true}}
```

---

## PYTHON AGENT LOOP (Fast)

```python
from anthropic import Anthropic; import subprocess, json, re

client = Anthropic()

def spec_to_rdf(req): # NL → RDF
  resp = client.messages.create(
    model="claude-opus-4-5", max_tokens=2048,
    messages=[{"role":"user", "content":f"Convert to Turtle RDF:\n{req}"}])
  return resp.content[0].text

def gen_code(ont, cfg="ggen.toml"): # RDF → code
  return json.loads(subprocess.run(
    ["ggen","sync","--config",cfg,"--format","json"],
    capture_output=True, text=True).stdout)

def validate(proj="."): # Test
  r = subprocess.run(["cargo","test"], cwd=proj, capture_output=True, text=True)
  return {"pass": r.returncode==0, "errors": re.findall(r"error\[", r.stderr)}

def refine(ont, fb): # Feedback → improved spec
  resp = client.messages.create(
    model="claude-opus-4-5", max_tokens=2048,
    messages=[{"role":"user","content":f"Fix spec for:\n{fb['errors']}"}])
  return resp.content[0].text

# Loop
for i in range(5):
  ont = spec_to_rdf(req) if i==0 else refine(ont, fb)
  gen = gen_code(ont)
  fb = validate()
  if fb["pass"]: break
  print(f"Iter {i}: {'✓' if fb['pass'] else '✗'}")
```

---

## GIT WORKFLOW

```bash
# Feature branch
git checkout -b claude/feature-name-aNIKP
git add docs/*.md
git commit -m "docs: Brief description"
git push -u origin claude/feature-name-aNIKP

# Pre-commit
cargo make check   # <5s
cargo make test    # <60s
cargo make lint    # clippy

# Push rules
git push -u origin branch
# Auto retries: 2s, 4s, 8s, 16s on network fail
```

---

## DECISION MATRIX

| Do? | Language | Framework | Example |
|-----|----------|-----------|---------|
| **REST API** | Rust | Actix | SPARQL select classes → Tera actix routes |
| | Python | FastAPI | SPARQL select classes → Tera FastAPI routes |
| | JS | Express | SPARQL select classes → Tera Express routes |
| **GraphQL** | Rust | Any | SPARQL select + relationships → GraphQL schema |
| **gRPC** | Rust/Go | Any | SPARQL select + types → .proto + service |
| **Schema** | SQL | Postgres | SPARQL classes → CREATE TABLE |
| **Tests** | Rust | - | SPARQL scenarios → #[test] cases |
| **Docs** | Markdown | - | SPARQL select + comments → MD |

---

## PERFORMANCE

| Ontology | Queries | Time | Opt |
|----------|---------|------|-----|
| <100 cls | Simple | <50ms | Direct |
| 100-500 | Medium | 100-300ms | OK |
| 500-2k | Complex | 500-2s | Pre-mat |
| >2k | Any | >2s | Split |

**Tips**: DISTINCT, MINUS, LIMIT early, no expensive OPTIONAL, use indexes

---

## ERROR CODES

| Code | Meaning | Fix |
|------|---------|-----|
| 0 | OK | - |
| 1 | Error | Check logs |
| 2 | Parse error | Check ggen.toml, .ttl syntax |
| 3 | Generation fail | Check SPARQL, templates |
| 4 | Validation fail | Check ontology constraints |
| 5 | Timeout | Increase --timeout, simplify queries |

---

## ACRONYMS

| A | = |
|---|---|
| TTL | Turtle (RDF syntax) |
| SPARQL | RDF query language |
| CONSTRUCT | Materialize facts |
| SELECT | Query results |
| ASK | Boolean query |
| SHACL | RDF validation |
| FMEA | Failure mode analysis |
| RPN | Risk priority number |
| MAPE-K | Monitoring/analysis/planning/execution/knowledge |
| DfLSS | Design for lean six sigma |
| TDD | Test-driven development |
| PII | Personally identifiable info |
| ORM | Object-relational mapper |
| DTO | Data transfer object |
| UUID | Universally unique identifier |
| JWT | JSON web token |
| gRPC | Google RPC |
| CRUD | Create/read/update/delete |

---

## QUICK LOOKUP

**ontology?** → TTL (RDF syntax) in `ontology/` → SPARQL rules → templates

**template?** → Tera in `templates/` → loops/conditionals/filters → output

**rule?** → SPARQL CONSTRUCT → materializes facts → templates see `:newProp`

**config?** → ggen.toml → project/generation/inference/lifecycle/marketplace/security

**error?** → Exit code → Check logs → Use -vvv → See error catalog

**slow?** → Profile SPARQL → Reduce results → Add LIMIT → Pre-materialize

**stuck?** → --validate-only → --dry-run → -vvv → check audit.json

