# Tutorial: Getting Started with Open Ontologies in ggen

Welcome! In this tutorial, you will learn how to initialize, validate, and generate code from an open ontology using the `ggen` CLI.

## Prerequisites

- You should have the `ggen` CLI installed.
- Basic understanding of Semantic Web concepts (RDF, OWL) is helpful but not required.

## Step 1: Initialize an Ontology Project

To begin working with ontologies, use the `init` command to scaffold a new project.

```bash
ggen ontology init my-ontology-project --template basic
```

This command creates a new directory called `my-ontology-project` containing an example ontology file (e.g., `example.ttl`) and a basic configuration. Open `example.ttl` to see standard definitions like `owl:Class` and `owl:DatatypeProperty`.

## Step 2: Validate the Schema Quality

Before generating code, you want to ensure that your ontology schema is semantically correct.

```bash
ggen ontology validate my-ontology-project/ontologies/example.ttl --strict
```

The validation tool analyzes the schema and reports:
- Whether the schema is valid.
- The number of classes and properties detected.
- Any warnings or errors (e.g., missing domains or ranges).

## Step 3: Generate Code from the Ontology

Once validated, you can automatically generate strongly-typed domain code (such as Rust or TypeScript structs) from the ontology schema.

```bash
ggen ontology generate my-ontology-project/ontologies/example.ttl rust --output ./src/generated --zod --utilities
```

This command will:
1. Extract the schema using the default namespace (`http://example.org#`).
2. Generate the necessary data models in your chosen target language.
3. Save the generated files to the `./src/generated` directory.

## Summary

You have successfully scaffolded an ontology project, validated its RDF schema, and used code generation to bridge the gap between semantic models and your application code.
