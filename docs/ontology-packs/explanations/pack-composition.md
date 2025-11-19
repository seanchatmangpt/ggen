# Understanding Pack Composition and Dependencies

Deep dive into how multiple ontologies work together.

---

## Why Composition?

Real applications need multiple ontologies:
- **E-commerce:** Schema.org (Product) + GoodRelations (pricing)
- **Social:** FOAF (people) + SIOC (online communities)
- **Publishing:** Schema.org (Article) + Dublin Core (metadata)

---

## Composition Strategies

### 1. Union (Combine All)

Merge all types from all packs:

```
Pack A: Person, Organization
Pack B: Event, Place
Result: Person, Organization, Event, Place
```

**Use case:** Need everything from both ontologies

---

### 2. Intersection (Common Only)

Keep only types present in all packs:

```
Pack A: Person, Organization, Event
Pack B: Person, Event, Place
Result: Person, Event
```

**Use case:** Find compatible subset

---

### 3. Extend (Base + Additions)

Start with base, add specific types:

```
Base (Schema.org): Person, Organization
Extension (FOAF): OnlineAccount, Group
Result: Person (enhanced), Organization, OnlineAccount, Group
```

**Use case:** Enhance base ontology

---

## Handling Conflicts

### Name Conflicts

Both Schema.org and FOAF have `Person`:

**Strategy 1: Namespace**
```typescript
interface SchemaPerson { ... }
interface FoafPerson { ... }
```

**Strategy 2: Merge**
```typescript
interface Person {
  // Properties from both
  name?: string;        // Schema.org
  knows?: Person[];     // FOAF
}
```

---

### Property Conflicts

Same property, different meanings:

**Strategy: Rename**
```yaml
property_name_overrides:
  "schema:email": "emailAddress"
  "foaf:mbox": "mailbox"
```

---

## Dependencies

Packs can depend on others:

```yaml
name: "my-ontology"
dependencies:
  - name: "schema.org"
    version: "^1.0.0"
  - name: "foaf"
    version: ">=1.0.0"
```

**Auto-resolution:**
```bash
ggen ontology install my-ontology
# Automatically installs schema.org and foaf
```

---

## Related Explanations

- [Case Study: Evolution](case-study-evolution.md)
- [Architecture: Marketplace](marketplace-architecture.md)
- [How to: Compose Ontologies](../how-to/compose-ontologies.md)
