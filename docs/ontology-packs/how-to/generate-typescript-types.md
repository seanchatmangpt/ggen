<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-To: Generate Advanced TypeScript Code](#how-to-generate-advanced-typescript-code)
  - [Basic Generation](#basic-generation)
  - [With Validation (Zod)](#with-validation-zod)
  - [With Utilities](#with-utilities)
  - [Custom Templates](#custom-templates)
  - [Comparing Generated Code](#comparing-generated-code)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-To: Generate Advanced TypeScript Code

## Basic Generation

```bash
ggen ontology generate \
  --schema my-ontology.ttl \
  --language typescript \
  --output src/generated
```

## With Validation (Zod)

```bash
ggen ontology generate \
  --schema my-ontology.ttl \
  --language typescript \
  --features "zod" \
  --output src/generated
```

This generates:
- `types.ts` - TypeScript interfaces
- `validators.ts` - Zod schemas for runtime validation

## With Utilities

```bash
ggen ontology generate \
  --schema my-ontology.ttl \
  --language typescript \
  --features "zod,utilities" \
  --output src/generated
```

Adds helper functions:
- Type guards
- Conversion utilities
- Serialization helpers

## Custom Templates

Use your own templates:

```bash
ggen ontology generate \
  --schema my-ontology.ttl \
  --language typescript \
  --template ./my-templates \
  --output src/generated
```

Template variables:
- `{{ class.name }}` - Class name
- `{{ class.properties }}` - Properties array
- `{{ class.description }}` - Documentation

## Comparing Generated Code

See what changed between versions:

```bash
ggen ontology generate \
  --schema my-ontology.ttl \
  --language typescript \
  --output src/generated \
  --compare-with-existing
```

Shows additions, removals, and modifications.
