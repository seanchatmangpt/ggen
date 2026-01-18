# Tutorial: Using Schema.org for E-Commerce Types

**Duration**: 15 minutes | **Level**: Intermediate | **Focus**: Real-world code generation

This tutorial shows how to extract the Schema.org vocabulary and generate TypeScript types for e-commerce applications.

## What You'll Learn

- Extract classes and properties from external vocabularies
- Generate usable TypeScript type definitions
- Apply generated types in your application
- Handle schema versioning and updates

## Step 1: Discover Schema.org Pack

```bash
ggen ontology discover schema-org
```

## Step 2: Extract E-Commerce Subset

```bash
ggen ontology extract \
  --pack schema-org \
  --namespace "https://schema.org/e-commerce" \
  --classes "Product,Offer,Organization,Person,Brand,Place" \
  --output schema-org-ecommerce.ttl
```

## Step 3: Generate TypeScript Types

```bash
ggen ontology generate \
  --schema schema-org-ecommerce.ttl \
  --language typescript \
  --features "zod,utilities" \
  --output src/generated
```

## Step 4: Use Generated Types

The generated types are production-ready with full validation support.

## Next Steps

- [Compose ontologies](../how-to/compose-ontologies.md)
- [Available ontologies](../reference/ontologies-registry.md)
