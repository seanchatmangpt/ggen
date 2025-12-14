<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-To: Publish Custom Ontology Packs](#how-to-publish-custom-ontology-packs)
  - [Creating a Pack](#creating-a-pack)
  - [Organizing Files](#organizing-files)
  - [Publishing](#publishing)
  - [Publishing Updates](#publishing-updates)
  - [Testing Before Publishing](#testing-before-publishing)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-To: Publish Custom Ontology Packs

## Creating a Pack

```bash
mkdir my-ontology
cd my-ontology
```

Create `gpack.toml`:

```toml
[package]
name = "my-ontology"
version = "1.0.0"
author = "Your Name"
description = "My custom ontology pack"
license = "MIT"

[ontology]
source = "ontology.ttl"
namespace = "https://mycompany.org/ontology/"

[ontology.targets]
typescript = { features = ["zod", "utilities"] }
rust = { features = ["serde", "validation"] }
python = { features = ["pydantic"] }

[templates]
typescript = "./templates/typescript/"
rust = "./templates/rust/"
python = "./templates/python/"
```

## Organizing Files

```
my-ontology/
├── gpack.toml
├── ontology.ttl              # Your RDF/OWL vocabulary
├── README.md                 # Pack documentation
├── templates/
│   ├── typescript/
│   │   ├── types.tera
│   │   └── validators.tera
│   ├── rust/
│   │   ├── types.tera
│   │   └── serde.tera
│   └── python/
│       └── models.tera
└── examples/
    └── usage.ts
```

## Publishing

```bash
ggen pack publish . --visibility public
```

Options:
- `--visibility public` - Available to all users
- `--visibility private` - Only for your account
- `--registry` - Custom registry URL

## Publishing Updates

Increment version in `gpack.toml`:

```toml
[package]
version = "1.1.0"  # Was 1.0.0
```

Then publish:

```bash
ggen pack publish . --visibility public
```

## Testing Before Publishing

```bash
ggen pack validate .
ggen ontology generate \
  --pack . \
  --language typescript \
  --output test-output
```

Verify generated code compiles:

```bash
cd test-output
npm install
npm run build
```
