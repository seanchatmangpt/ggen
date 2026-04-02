<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [SPARQL Pattern Reference (Compressed)](#sparql-pattern-reference-compressed)
  - [SELECT Patterns](#select-patterns)
  - [CONSTRUCT Patterns (Materialization)](#construct-patterns-materialization)
  - [ASK Pattern](#ask-pattern)
  - [Common Patterns (Copy-Paste Ready)](#common-patterns-copy-paste-ready)
    - [Inheritance Closure](#inheritance-closure)
    - [Property Domain Expansion](#property-domain-expansion)
    - [Capability Aggregation](#capability-aggregation)
    - [Validation Rule Extraction](#validation-rule-extraction)
    - [Find Unused Classes](#find-unused-classes)
    - [Circular Dependencies](#circular-dependencies)
    - [Cross-Class Property Reuse](#cross-class-property-reuse)
  - [Performance Tips](#performance-tips)
  - [Template Integration Quick Map](#template-integration-quick-map)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# SPARQL Pattern Reference (Compressed)

> All SPARQL patterns on 2 pages. No fluff.

## SELECT Patterns

| Pattern | Query | Use |
|---------|-------|-----|
| **Find by Type** | `?x a rdfs:Class` | List all entities of type |
| **Property Match** | `?x prop ?y . FILTER (?y = "value")` | Find by property value |
| **Optional** | `?x prop1 ?y . OPTIONAL { ?x prop2 ?z }` | May or may not exist |
| **Union** | `{ ?x a ex:User } UNION { ?x a ex:Admin }` | OR conditions |
| **Transitive** | `?x rdfs:subClassOf+ ?y` | All ancestors (+ = 1+, * = 0+) |
| **Negation** | `MINUS { ?x a ex:Deleted }` | Exclude matches |
| **Aggregate** | `SELECT ?x (COUNT(?y) AS ?cnt) GROUP BY ?x` | Group/count results |
| **Filter Math** | `FILTER (?age >= 18 && ?age <= 65)` | Numeric/logical filters |
| **Regex** | `FILTER regex(?name, "^A", "i")` | Case-insensitive pattern match |
| **Bind** | `BIND (STR(?x) AS ?name)` | Transform/compute values |
| **Having** | `GROUP BY ?x HAVING (COUNT(?y) > 5)` | Filter groups |

## CONSTRUCT Patterns (Materialization)

| Pattern | Effect | When |
|---------|--------|------|
| **Transitive Closure** | `?a :rel+ ?c` → `?a :allRels ?c` | Flatten hierarchies |
| **Property Expansion** | Domain/range inherited to subclasses | Property inference |
| **Role Materialization** | Capabilities → interface properties | Interface extraction |
| **Derived Facts** | Computed from rules → stored as triples | Caching inferences |
| **Normalization** | Multiple equivalent forms → canonical form | Consistency |
| **De-duplication** | MINUS to skip materialized facts | Idempotent rules |

## ASK Pattern

| Query | Result | Use |
|-------|--------|-----|
| `ASK { ?x a rdfs:Class }` | true/false | Does class exist? |
| `ASK { ?x a rdfs:Class . FILTER NOT EXISTS { ?x rdfs:label ?l } }` | true/false | Undocumented class? |

---

## Common Patterns (Copy-Paste Ready)

### Inheritance Closure
```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
CONSTRUCT { ?sub :allSuperclasses ?super . }
WHERE { ?sub rdfs:subClassOf+ ?super . }
```

### Property Domain Expansion
```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
CONSTRUCT { ?prop rdfs:domain ?sub . }
WHERE { ?prop rdfs:domain ?super . ?sub rdfs:subClassOf+ ?super .
  MINUS { ?prop rdfs:domain ?sub . } }
```

### Capability Aggregation
```sparql
PREFIX ex: <https://example.com/>
CONSTRUCT { ?class :hasCapability ?cap . }
WHERE { { ?class :canCreate ?type . BIND("Create" AS ?cap) }
  UNION { ?class :canRead ?type . BIND("Read" AS ?cap) }
  UNION { ?class :canUpdate ?type . BIND("Update" AS ?cap) }
  UNION { ?class :canDelete ?type . BIND("Delete" AS ?cap) }
  UNION { ?sup :hasCapability ?cap . ?class rdfs:subClassOf+ ?sup . } }
```

### Validation Rule Extraction
```sparql
PREFIX sh: <http://www.w3.org/ns/shacl#>
CONSTRUCT { ?prop :minLength ?min . ?prop :maxLength ?max . ?prop :pattern ?pat . }
WHERE { ?shape sh:property [ sh:path ?prop ;
    sh:minLength ?min ; sh:maxLength ?max ; sh:pattern ?pat ] . }
```

### Find Unused Classes
```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?class WHERE { ?class a rdfs:Class .
  FILTER NOT EXISTS { ?inst a ?class . ?class rdfs:subClassOf ?sub . } }
```

### Circular Dependencies
```sparql
PREFIX ex: <https://example.com/>
SELECT ?a ?b ?c WHERE { ?a ex:dependsOn+ ?b . ?b ex:dependsOn+ ?c .
  ?c ex:dependsOn+ ?a . }
```

### Cross-Class Property Reuse
```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?prop (COUNT(?class) AS ?cnt) WHERE {
  ?prop rdfs:domain ?class . }
GROUP BY ?prop HAVING (COUNT(?class) > 3) ORDER BY DESC(?cnt)
```

---

## Performance Tips

| Problem | Solution |
|---------|----------|
| Slow queries | Use DISTINCT, MINUS, add LIMIT |
| Too many results | Add FILTER, MINUS, or LIMIT early |
| Missing transitive | Use `+` (1+) or `*` (0+) in relations |
| Expensive optionals | Move optional to end, use MINUS |
| Ambiguous results | Add more specific FILTER conditions |
| Large graphs (1000+) | Pre-materialize with CONSTRUCT, use indexes |

---

## Template Integration Quick Map

```jinja2
# Access materialized facts
{% for class in classes %}
  {{ class.allSuperclasses }}  {# from inheritance_closure rule #}
  {{ class.hasCapability }}    {# from role_materialization rule #}
  {{ class.minLength }}        {# from validation_extraction rule #}
{% endfor %}
```

