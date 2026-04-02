<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Tutorial: Using Schema.org for E-Commerce Types](#tutorial-using-schemaorg-for-e-commerce-types)
  - [What You'll Learn](#what-youll-learn)
  - [Step 1: Discover Schema.org Pack](#step-1-discover-schemaorg-pack)
  - [Step 2: Extract E-Commerce Subset](#step-2-extract-e-commerce-subset)
  - [Step 3: Generate TypeScript Types](#step-3-generate-typescript-types)
  - [Step 4: Use Generated Types](#step-4-use-generated-types)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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
