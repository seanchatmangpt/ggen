<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-To: Compose Multiple Ontologies](#how-to-compose-multiple-ontologies)
  - [Combining Vocabularies](#combining-vocabularies)
  - [Real-World Example](#real-world-example)
  - [Conflict Resolution](#conflict-resolution)
  - [Validation After Composition](#validation-after-composition)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-To: Compose Multiple Ontologies

## Combining Vocabularies

Merge two or more ontologies:

```bash
ggen ontology compose \
  --packs schema-org,dublin-core \
  --merge-strategy union \
  --output combined-ontology.ttl
```

Merge strategies:
- `union` - Combine all classes and properties
- `intersection` - Only common elements
- `priority` - First pack takes precedence in conflicts

## Real-World Example

Combine Schema.org (e-commerce) with Dublin Core (metadata):

```bash
ggen ontology compose \
  --packs "schema-org::Product,Organization" \
  --packs "dublin-core::Creator,Date,Subject" \
  --merge-strategy union \
  --namespace "https://mycompany.org/ontology/" \
  --output mycompany-ontology.ttl
```

Then generate code:

```bash
ggen ontology generate \
  --schema mycompany-ontology.ttl \
  --language typescript \
  --output src/generated
```

## Conflict Resolution

When classes overlap, specify resolution:

```bash
ggen ontology compose \
  --packs schema-org,foaf \
  --conflicts resolve \
  --conflict-mode "first-wins" \
  --output merged.ttl
```

## Validation After Composition

Ensure composed ontology is valid:

```bash
ggen ontology validate \
  --schema merged.ttl \
  --strict
```

Checks:
- No circular dependencies
- Property types consistency
- Namespace conflicts
- Missing cardinality constraints
