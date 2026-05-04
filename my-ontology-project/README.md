# my-ontology-project - Ontology Project

Auto-generated ontology project using ggen.

## Quick Start

```bash
# Extract ontology schema
npm run extract

# Generate TypeScript code from ontology
npm run generate

# Validate ontology quality
npm run validate
```

## Project Structure

- `ontologies/` - RDF/OWL ontology files (Turtle, RDF/XML, etc.)
- `src/` - Source code and generated types
- `generated/` - Generated artifacts (TypeScript, GraphQL, SQL, etc.)
- `ggen.config.json` - Configuration for code generation

## Available Commands

### Extract Schema
```bash
ggen ontology extract ontologies/schema.ttl [--namespace <uri>] [--output <file>]
```

### Generate Code
```bash
ggen ontology generate schema.json [--language typescript] [--zod] [--utilities]
```

### Validate Quality
```bash
ggen ontology validate schema.json [--strict]
```

## Supported Ontology Formats

- **Turtle** (.ttl) - Recommended, most human-readable
- **RDF/XML** (.rdf, .xml)
- **N-Triples** (.nt)

## Supported Code Generation Targets

- TypeScript interfaces with Zod validation schemas
- (GraphQL, React components, SQL coming soon)

## Example Ontologies

This project includes example ontologies:
- `schema-org-example.ttl` - Schema.org subset
- `foaf-example.ttl` - Friend of a Friend vocabulary
- `dublincore-example.ttl` - Dublin Core metadata

## Resources

- [RDF Concepts](https://www.w3.org/TR/rdf-concepts/)
- [SPARQL Query Language](https://www.w3.org/TR/sparql11-query/)
- [OWL 2 Web Ontology Language](https://www.w3.org/TR/owl2-overview/)
