<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-To: Manage Ontology Pack Versions](#how-to-manage-ontology-pack-versions)
  - [Understanding Semantic Versioning](#understanding-semantic-versioning)
  - [Checking for Updates](#checking-for-updates)
  - [Updating Packs](#updating-packs)
  - [Version Compatibility](#version-compatibility)
  - [Lockfiles](#lockfiles)
  - [Tracking Changes](#tracking-changes)
  - [Migration Guide](#migration-guide)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-To: Manage Ontology Pack Versions

## Understanding Semantic Versioning

Ontology packs follow semver: `MAJOR.MINOR.PATCH`

- `MAJOR` - Breaking changes (removed classes/properties)
- `MINOR` - Backward-compatible additions
- `PATCH` - Bug fixes and corrections

## Checking for Updates

```bash
ggen ontology list --check-updates
```

Shows:
```
schema-org  3.12.0 -> 3.13.0 (1 minor update available)
foaf        1.0.0  (up to date)
```

## Updating Packs

Update to latest:

```bash
ggen ontology update schema-org --version latest
```

Update to specific version:

```bash
ggen ontology update schema-org --version 3.13.0
```

## Version Compatibility

When a pack has breaking changes:

```bash
ggen ontology generate \
  --schema my-ontology.ttl \
  --compatibility-version 3.12.0 \
  --output src/generated
```

This generates code compatible with the specified version.

## Lockfiles

Create a lockfile to freeze versions:

```bash
ggen ontology lock create
```

This generates `ontology-lock.toml`:

```toml
[[packs]]
name = "schema-org"
version = "3.13.0"
resolved = "registry://ggen-marketplace/schema-org@3.13.0"

[[packs]]
name = "dublin-core"
version = "1.11.0"
resolved = "registry://ggen-marketplace/dublin-core@1.11.0"
```

Use locked versions:

```bash
ggen ontology generate --use-lock-file ontology-lock.toml
```

## Tracking Changes

View what changed between versions:

```bash
ggen ontology diff schema-org 3.12.0 3.13.0
```

Shows:
```
Added classes:     ReviewRating, AggregateRating
Modified classes:  Product (4 new properties)
Removed classes:   None
Deprecated:        DeprecatedClass (replaced by NewClass)
```

## Migration Guide

When updating to a new major version:

```bash
ggen ontology generate \
  --schema my-ontology.ttl \
  --language typescript \
  --compare-with-existing \
  --output src/generated
```

Review the diff and update your code accordingly.
