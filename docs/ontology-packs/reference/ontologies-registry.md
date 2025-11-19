# Reference: Available Ontologies in Marketplace

Popular ontology packs available for code generation.

## General Purpose

### Schema.org
The most comprehensive general vocabulary with 788+ classes.

```bash
ggen ontology install schema-org
```

**Details**:
- Classes: 788+
- Properties: 2,500+
- Domains: All (e-commerce, articles, events, health, etc.)
- Languages: TypeScript, Rust, Python, Go, Java, C#
- Rating: ⭐⭐⭐⭐⭐ (2,340 votes)
- Downloads: 45.2K

**Key Classes**: Product, Article, Person, Organization, Event, Place

### DBpedia
Structured data from Wikipedia with billions of instances.

```bash
ggen ontology install dbpedia
```

**Details**:
- Classes: 700+
- Properties: 4,000+
- Focus: Knowledge base, entities
- Rating: ⭐⭐⭐⭐ (890 votes)

## Metadata & Documentation

### Dublin Core
Standard metadata vocabulary for library and document management.

```bash
ggen ontology install dublin-core
```

**Details**:
- Classes: 35
- Properties: 50+
- Focus: Document metadata
- Rating: ⭐⭐⭐⭐⭐ (1,200 votes)

**Key Properties**: Creator, Date, Subject, Title, Type

### MIAOU (Metadata Infrastructure for Audiovisual Objects)
Metadata for video and audio content.

```bash
ggen ontology install miaou
```

## Social & People

### FOAF (Friend of a Friend)
Social network and person/organization vocabulary.

```bash
ggen ontology install foaf
```

**Details**:
- Classes: 35+
- Properties: 60+
- Focus: Social networks, people
- Rating: ⭐⭐⭐⭐ (1,100 votes)

**Key Classes**: Person, Organization, Project

## Domain-Specific

### SKOS (Simple Knowledge Organization System)
Vocabulary for knowledge organization and taxonomies.

```bash
ggen ontology install skos
```

### BioOntology
Biological and medical terms and concepts.

```bash
ggen ontology install bio-ontology
```

### GEO
Geographic data and spatial relationships.

```bash
ggen ontology install geo
```

### VCARD
Contact and organization information.

```bash
ggen ontology install vcard
```

## E-Commerce (Curated)

### ecommerce-ontology
Pre-composed pack with Schema.org + Dublin Core for shopping.

```bash
ggen ontology install ecommerce-ontology
```

**Includes**: Product, Offer, Order, PaymentMethod, ShippingMethod

### inventory-ontology
Inventory management and supply chain.

```bash
ggen ontology install inventory-ontology
```

## Healthcare

### HL7 FHIR
Fast Healthcare Interoperability Resources standard.

```bash
ggen ontology install hl7-fhir
```

## Publishing & Content

### DCAT
Data Catalog Vocabulary for data portals.

```bash
ggen ontology install dcat
```

### CIDOC-CRM
Cultural Heritage and Museum ontology.

```bash
ggen ontology install cidoc-crm
```

## Installation Tips

List all available packs:
```bash
ggen ontology discover --limit 100
```

Check pack details before installing:
```bash
ggen ontology info schema-org --verbose
```

Search for specific domain:
```bash
ggen ontology discover --domain ecommerce --limit 20
```

View installed packs:
```bash
ggen ontology list --installed
```
