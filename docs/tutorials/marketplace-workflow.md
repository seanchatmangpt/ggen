# Marketplace Workflow

**Goal:** Discover, install, and use marketplace templates to accelerate development.

**What you'll learn:** How to find templates, install them, and generate code from marketplace packages.

## Step 1: Search for Templates

Find templates that match your needs:

```bash
ggen marketplace search "rust microservice"
```

**Output:**
```
Found 15 packages:
  io.ggen.rust.microservice (v1.2.0) - Rust microservice template
  io.ggen.rust.axum-api (v2.0.0) - Axum REST API template
  io.ggen.rust.graphql (v1.5.0) - GraphQL API template
  ...
```

## Step 2: Install a Template

Install the template you need:

```bash
ggen marketplace install io.ggen.rust.microservice
```

This downloads and installs the template to your local cache.

## Step 3: Use the Template

Generate code using the installed template:

```bash
ggen project gen my-service \
  --template io.ggen.rust.microservice \
  --vars name=user-service,port=8080
```

## Step 4: Customize with Your Ontology

Combine marketplace templates with your domain ontology:

```bash
# Create your domain ontology
ggen ai generate-ontology \
  --prompt "User management: User, Role, Permission" \
  --output domain.ttl

# Generate using marketplace template + your ontology
ggen template generate-rdf \
  --ontology domain.ttl \
  --template io.ggen.rust.microservice \
  --output src/
```

## Marketplace Benefits

1. **Proven templates:** Battle-tested code patterns
2. **Fast setup:** Skip boilerplate, start with working code
3. **Customizable:** Combine with your ontologies
4. **Community:** Share and discover templates

## Advanced: Publishing Your Own Templates

Create and publish templates to the marketplace:

```bash
# Create template package
ggen marketplace publish \
  --name "my-rust-api-template" \
  --version "1.0.0" \
  --template-dir ./templates
```

## Next Steps

- **Marketplace guide:** [Marketplace Explanation](../explanations/marketplace.md)
- **Template creation:** [Create Templates Guide](../how-to-guides/create-templates.md)
- **CLI reference:** [Marketplace Commands](../reference/cli.md#marketplace-commands)

