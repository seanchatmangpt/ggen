<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Reference: Pack Metadata Specification](#reference-pack-metadata-specification)
  - [gpack.toml Structure](#gpacktoml-structure)
    - [&#91;package&#93; Section](#package-section)
    - [&#91;ontology&#93; Section](#ontology-section)
    - [&#91;templates&#93; Section](#templates-section)
    - [&#91;dependencies&#93; Section](#dependencies-section)
  - [Complete Example](#complete-example)
  - [Metadata Constraints](#metadata-constraints)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Reference: Pack Metadata Specification

## gpack.toml Structure

### [package] Section

```toml
[package]
name = "my-ontology"              # Required: lowercase, alphanumeric, hyphens
version = "1.0.0"                 # Required: semantic versioning
author = "Your Name"              # Required
description = "Short description" # Required
license = "MIT"                   # Required: SPDX license identifier
repository = "https://github.com/user/my-ontology"
documentation = "https://docs.example.com"
homepage = "https://example.com"
keywords = ["ontology", "schema"]
categories = ["ecommerce", "semantic-web"]
```

### [ontology] Section

```toml
[ontology]
source = "ontology.ttl"           # Path to RDF/OWL file
namespace = "https://..."         # Base namespace URI
format = "turtle"                 # turtle, rdfxml, ntriples, jsonld
sparql_endpoint = "http://..."    # Optional: live SPARQL endpoint

[ontology.targets]
typescript = { features = ["zod", "utilities"] }
rust = { features = ["serde", "validation"] }
python = { features = ["pydantic"] }
```

### [templates] Section

```toml
[templates]
typescript = "./templates/typescript/"
rust = "./templates/rust/"
python = "./templates/python/"
```

### [dependencies] Section

```toml
[dependencies]
schema-org = "3.13.0"
dublin-core = "1.11.0"
```

## Complete Example

```toml
[package]
name = "ecommerce-ontology"
version = "1.2.0"
author = "E-Commerce Team <team@example.com>"
description = "E-commerce vocabulary for product catalogs"
license = "CC-BY-4.0"
repository = "https://github.com/example/ecommerce-ontology"
documentation = "https://docs.example.com/ontology"
homepage = "https://example.com"
keywords = ["ontology", "ecommerce", "schema"]
categories = ["ecommerce", "semantic-web"]

[ontology]
source = "ontology/ecommerce.ttl"
namespace = "https://example.com/ecommerce/"
format = "turtle"

[ontology.targets]
typescript = { features = ["zod", "utilities", "graphql"] }
rust = { features = ["serde", "validation", "sqlx"] }
python = { features = ["pydantic", "sqlalchemy"] }
go = { features = ["json", "validation"] }

[templates]
typescript = "./templates/typescript/"
rust = "./templates/rust/"
python = "./templates/python/"
go = "./templates/go/"

[dependencies]
schema-org = ">=3.0.0, <4.0.0"
dublin-core = "~1.11.0"
```

## Metadata Constraints

- `name`: 1-64 characters, lowercase, must match `^[a-z0-9]([a-z0-9-]{0,62}[a-z0-9])?$`
- `version`: Must be valid semver (major.minor.patch)
- `license`: Must be valid SPDX identifier
- `namespace`: Must be valid URI
- `categories`: Maximum 5 categories
- `keywords`: Maximum 10 keywords
