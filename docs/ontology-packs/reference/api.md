<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [API Reference](#api-reference)
  - [TypeScript/JavaScript API](#typescriptjavascript-api)
    - [Install from npm](#install-from-npm)
    - [Load Ontology Pack](#load-ontology-pack)
    - [Generate Code](#generate-code)
    - [Compose Packs](#compose-packs)
  - [Rust API](#rust-api)
    - [Add to Cargo.toml](#add-to-cargotoml)
    - [Load Pack](#load-pack)
    - [Generate Code](#generate-code-1)
  - [Related References](#related-references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# API Reference

Programmatic API for ontology pack operations.

---

## TypeScript/JavaScript API

### Install from npm

```bash
npm install @ggen/ontology-packs
```

---

### Load Ontology Pack

```typescript
import { OntologyPack } from '@ggen/ontology-packs';

const pack = await OntologyPack.load('schema.org');
```

---

### Generate Code

```typescript
const output = await pack.generate({
  template: 'typescript',
  outputDir: './src/types',
  config: {
    strict_null_checks: true,
    include_validators: true
  }
});
```

---

### Compose Packs

```typescript
import { composePacks } from '@ggen/ontology-packs';

const composed = await composePacks({
  packs: ['schema.org', 'foaf'],
  strategy: 'union'
});
```

---

## Rust API

### Add to Cargo.toml

```toml
[dependencies]
ggen-ontology = "1.0.0"
```

---

### Load Pack

```rust
use ggen_ontology::OntologyPack;

let pack = OntologyPack::load("schema.org")?;
```

---

### Generate Code

```rust
let output = pack.generate(GenerateOptions {
    template: "rust".to_string(),
    output_dir: "./src/types".into(),
    config: Some(config),
})?;
```

---

## Related References

- [CLI Command Reference](cli-commands.md)
- [Template Variable Reference](template-variables.md)
