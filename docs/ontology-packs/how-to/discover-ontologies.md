<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-To: Discover and Search Ontology Packs](#how-to-discover-and-search-ontology-packs)
  - [Searching the Marketplace](#searching-the-marketplace)
  - [Popular Ontologies](#popular-ontologies)
    - [Schema.org](#schemaorg)
    - [Dublin Core](#dublin-core)
    - [FOAF (Friend of a Friend)](#foaf-friend-of-a-friend)
  - [Installing Packs](#installing-packs)
  - [Listing Installed Packs](#listing-installed-packs)
  - [Checking Pack Details](#checking-pack-details)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-To: Discover and Search Ontology Packs

## Searching the Marketplace

Find available ontology packs with search:

```bash
ggen ontology discover --query "schema" --limit 20
```

Filters:
- `--domain` - Filter by domain (e-commerce, healthcare, etc.)
- `--language` - Filter by target language
- `--min-rating` - Minimum star rating
- `--sort` - Sort by downloads, rating, or recency

## Popular Ontologies

### Schema.org
The most widely used vocabulary with 788+ classes.

```bash
ggen ontology install schema-org
```

### Dublin Core
Metadata vocabulary for libraries and archives.

```bash
ggen ontology install dublin-core
```

### FOAF (Friend of a Friend)
Social network and person/organization vocabulary.

```bash
ggen ontology install foaf
```

## Installing Packs

```bash
ggen ontology install <pack-name> --version latest
```

## Listing Installed Packs

```bash
ggen ontology list --installed
```

## Checking Pack Details

```bash
ggen ontology info schema-org --verbose
```
