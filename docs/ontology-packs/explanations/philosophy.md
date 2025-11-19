# The Philosophy of Reusable Semantic Assets

Core principles behind the ontology-as-packs system.

---

## Principle 1: Data and Templates Are Separate Concerns

**Ontologies are data** - RDF graphs describing concepts
**Templates are presentation** - How to render that data as code

**Why separate?**
- One ontology → Many output formats (TypeScript, Python, Rust)
- One template → Many ontologies (reusable patterns)
- Users can customize presentation without touching data

---

## Principle 2: Share Knowledge, Not Code

**Old model:** Every developer writes their own Schema.org parser
**New model:** Community maintains ONE Schema.org pack

**Benefits:**
- No duplicated effort
- Community improvements benefit everyone
- Best practices are encoded in templates

---

## Principle 3: Composition Over Inheritance

**Don't:**
```
Create mega-ontology with everything
```

**Do:**
```
Compose small, focused ontologies as needed
schema.org + foaf + dublin-core
```

**Why?**
- Flexibility
- Reusability
- Clear boundaries

---

## Principle 4: Explicit is Better Than Implicit

**Versioning:** Always explicit (`schema.org@1.0.0`)
**Dependencies:** Declared in pack.yaml
**Type mappings:** Configured, not guessed
**Conflicts:** Resolved explicitly

---

## Principle 5: Make the Right Thing Easy

**Easy:** Install and use vetted ontology packs
**Harder:** Create custom pack (but still supported)
**Hardest:** Parse RDF manually (but possible)

**The system guides users toward best practices while allowing flexibility.**

---

## Related Explanations

- [Case Study: Evolution](case-study-evolution.md)
- [Why Ontologies Matter](why-ontologies.md)
- [Understanding Pack Composition](pack-composition.md)
