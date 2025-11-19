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
