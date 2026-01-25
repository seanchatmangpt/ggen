<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Reference: CLI Commands](#reference-cli-commands)
  - [Discovery Commands](#discovery-commands)
    - [discover](#discover)
    - [info](#info)
  - [Installation Commands](#installation-commands)
    - [install](#install)
    - [update](#update)
    - [list](#list)
    - [uninstall](#uninstall)
  - [Generation Commands](#generation-commands)
    - [extract](#extract)
    - [generate](#generate)
    - [compose](#compose)
    - [validate](#validate)
  - [Lock File Commands](#lock-file-commands)
    - [lock create](#lock-create)
    - [lock update](#lock-update)
  - [Publishing Commands](#publishing-commands)
    - [pack publish](#pack-publish)
    - [pack validate](#pack-validate)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Reference: CLI Commands

## Discovery Commands

### discover
Search and discover ontology packs in the marketplace.

```bash
ggen ontology discover [OPTIONS] [QUERY]
```

Options:
- `--query TEXT` - Search terms
- `--domain TEXT` - Filter by domain
- `--language TEXT` - Filter by target language
- `--min-rating NUM` - Minimum rating (0-5)
- `--limit NUM` - Limit results (default: 10)
- `--sort FIELD` - Sort by downloads|rating|recency
- `--json` - Output as JSON

Examples:
```bash
ggen ontology discover schema
ggen ontology discover --domain ecommerce --language typescript
ggen ontology discover --sort downloads --limit 50
```

### info
Display detailed information about a pack.

```bash
ggen ontology info <pack-name> [OPTIONS]
```

Options:
- `--version TEXT` - Specific version (default: latest)
- `--verbose` - Show detailed statistics
- `--json` - Output as JSON

## Installation Commands

### install
Install an ontology pack.

```bash
ggen ontology install <pack-name> [OPTIONS]
```

Options:
- `--version TEXT` - Specific version (default: latest)
- `--force` - Overwrite existing pack
- `--no-cache` - Skip cache and download fresh

### update
Update installed packs.

```bash
ggen ontology update [PACK-NAME] [OPTIONS]
```

Options:
- `--version TEXT` - Update to specific version
- `--check` - Check for updates without installing
- `--all` - Update all packs

### list
List installed ontology packs.

```bash
ggen ontology list [OPTIONS]
```

Options:
- `--installed` - Show only installed packs
- `--check-updates` - Check for available updates
- `--json` - Output as JSON

### uninstall
Remove an installed pack.

```bash
ggen ontology uninstall <pack-name>
```

## Generation Commands

### extract
Extract classes and properties from ontology files.

```bash
ggen ontology extract [OPTIONS]
```

Options:
- `--pack TEXT` - Pack name or path
- `--namespace TEXT` - Namespace to extract
- `--classes TEXT` - Comma-separated class names to extract
- `--output PATH` - Output file path
- `--format TEXT` - Output format (turtle|rdfxml)

### generate
Generate code from ontology schemas.

```bash
ggen ontology generate [OPTIONS]
```

Options:
- `--schema PATH` - Path to ontology file
- `--language TEXT` - Target language (typescript|rust|python|go)
- `--features TEXT` - Comma-separated feature flags
- `--output PATH` - Output directory
- `--template PATH` - Custom template directory
- `--compare-with-existing` - Show differences from existing code

### compose
Combine multiple ontologies.

```bash
ggen ontology compose [OPTIONS]
```

Options:
- `--packs TEXT` - Comma-separated pack names
- `--merge-strategy TEXT` - union|intersection|priority
- `--namespace TEXT` - Output namespace
- `--output PATH` - Output file path

### validate
Validate ontology pack structure and content.

```bash
ggen ontology validate <pack-path> [OPTIONS]
```

Options:
- `--strict` - Fail on warnings
- `--json` - Output as JSON
- `--fix` - Attempt to fix validation errors

## Lock File Commands

### lock create
Create a lock file freezing current pack versions.

```bash
ggen ontology lock create [OPTIONS]
```

Options:
- `--output PATH` - Lock file path (default: ontology-lock.toml)

### lock update
Update locked pack versions.

```bash
ggen ontology lock update [OPTIONS]
```

## Publishing Commands

### pack publish
Publish pack to marketplace.

```bash
ggen pack publish <path> [OPTIONS]
```

Options:
- `--visibility public|private` - Pack visibility
- `--registry URL` - Registry URL
- `--force` - Skip validation

### pack validate
Validate pack structure.

```bash
ggen pack validate <path>
```
