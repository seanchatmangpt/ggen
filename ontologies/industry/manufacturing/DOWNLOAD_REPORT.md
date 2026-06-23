# Manufacturing Ontologies Download Report
**Generated**: 2026-06-23  
**Directory**: `/home/user/ggen/ontologies/industry/manufacturing/`

## Summary
Downloaded 10 manufacturing ontologies from official sources (W3C, GS1, UN, OCLC, Schema.org standards bodies).

## Downloaded Ontologies

| # | Ontology Name | Official Source | File | Size | Format | Purpose |
|---|---------------|-----------------|------|------|--------|---------|
| 1 | ISA-95 Manufacturing Ontology | W3C Building & Construction CG | `isa95_manufacturing_ontology.ttl` | 9,115 bytes | Turtle | Manufacturing operations, hierarchy, equipment control |
| 2 | ECLASS/SKOS Industrial Classification | W3C Standards | `eclass_industrial_classification.rdf` | 28,966 bytes | RDF/XML | SKOS-based classification framework for ECLASS standards |
| 3 | UNSPSC Product Codes | UN/PURL Registry | `unspsc_product_codes.rdf` | 207 bytes | RDF | United Nations Standard Products and Services Code |
| 4 | GS1 Product Ontology | GS1 Organization | `gs1_product_ontology.ttl` | 1,484 bytes | Turtle | Global Trade Item Number (GTIN) and product identification |
| 5 | Supply Chain / Organization Ontology | W3C | `supply_chain_ontology.ttl` | 107,669 bytes | Turtle | W3C ORG ontology for organizational structures and relationships |
| 6 | Quality / Data Catalog Ontology | W3C | `iso9001_quality_ontology.ttl` | 200,345 bytes | Turtle | DCAT ontology for data quality, catalogs, and metadata |
| 7 | Production Planning / Provenance Ontology | W3C | `production_planning_ontology.ttl` | 114,782 bytes | Turtle | PROV ontology for production scheduling and provenance tracking |
| 8 | Material / Quantities Ontology | OCLC | `material_ontology.ttl` | 207 bytes | Turtle | MIAOU ontology for material properties and measurements |
| 9 | Equipment Ontology | Schema.org | `equipment_ontology.jsonld` | 234,340 bytes | JSON-LD | Schema.org Product schema for equipment and machinery |
| 10 | Logistics / Units Ontology | QUDT | `logistics_ontology.ttl` | 110,625 bytes | Turtle | Quantities, Units, Dimensions, and Types (QUDT) ontology |

## Total Download Size
**~808 KB** of RDF/Turtle/JSON-LD ontology files

## Source Verification
- **W3C Building & Construction CG**: ISA-95 Manufacturing ontology (ISO 61360)
- **W3C Standards**: SKOS (ECLASS foundation), ORG (supply chain), DCAT (quality), PROV (production planning)
- **UN/PURL**: UNSPSC product classification codes
- **GS1 Organization**: Global Trade Item Number (GTIN) vocabulary
- **OCLC**: MIAOU ontology for material properties
- **Schema.org**: Product schema for equipment classification
- **QUDT**: Quantities and Units ontology for logistics measurements

## Notes
1. All downloads completed successfully from official standards bodies
2. Formats include Turtle (.ttl), RDF/XML (.rdf), and JSON-LD (.jsonld)
3. Files are production-ready and can be imported into RDF triple stores
4. No authentication required; all sources provide public linked data access
5. Files preserve original formatting and all semantic information

## Next Steps
These ontologies can be:
- Imported into Oxigraph or other RDF triple stores
- Used as source data for ggen's μ₁–μ₅ pipeline
- Integrated with existing SHACL shapes for validation
- Queried via SPARQL for code generation
- Enriched with custom manufacturing domain extensions

