<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Code Generation Options Reference](#code-generation-options-reference)
  - [Basic Usage](#basic-usage)
  - [Required Arguments](#required-arguments)
    - [`<pack>`](#pack)
  - [Options](#options)
    - [`--template <NAME>`](#--template-name)
    - [`--output <DIR>`](#--output-dir)
    - [`--config <JSON|FILE>`](#--config-jsonfile)
    - [`--filter <CLASSES>`](#--filter-classes)
    - [`--exclude <CLASSES>`](#--exclude-classes)
    - [`--force`](#--force)
    - [`--dry-run`](#--dry-run)
    - [`--verbose`](#--verbose)
  - [Complete Example](#complete-example)
  - [Related References](#related-references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Code Generation Options Reference

Complete CLI options for `ggen ontology generate` command.

---

## Basic Usage

```bash
ggen ontology generate <pack> [OPTIONS]
```

---

## Required Arguments

### `<pack>`
Pack name or path to pack directory/file

**Examples:**
```bash
ggen ontology generate schema.org
ggen ontology generate ./my-pack
ggen ontology generate ./my-pack.gpack
```

---

## Options

### `--template <NAME>`
Template to use for generation

**Example:**
```bash
--template typescript
--template ./custom-template
```

---

### `--output <DIR>`
Output directory for generated files

**Example:**
```bash
--output ./src/types
```

---

### `--config <JSON|FILE>`
Configuration JSON or path to config file

**Example:**
```bash
--config '{"strict_null_checks": true}'
--config ./template-config.yaml
```

---

### `--filter <CLASSES>`
Comma-separated list of classes to generate

**Example:**
```bash
--filter "Person,Organization,Event"
```

---

### `--exclude <CLASSES>`
Comma-separated list of classes to exclude

**Example:**
```bash
--exclude "Thing,Intangible"
```

---

### `--force`
Overwrite existing files

**Example:**
```bash
--force
```

---

### `--dry-run`
Preview generation without writing files

**Example:**
```bash
--dry-run
```

---

### `--verbose`
Enable verbose logging

**Example:**
```bash
--verbose
```

---

## Complete Example

```bash
ggen ontology generate schema.org \
  --template typescript \
  --output ./src/schema \
  --config '{
    "strict_null_checks": true,
    "include_validators": true,
    "validation_library": "zod"
  }' \
  --filter "Person,Organization,Event" \
  --force \
  --verbose
```

---

## Related References

- [Template Variable Reference](template-variables.md)
- [CLI Command Reference](cli-commands.md)
