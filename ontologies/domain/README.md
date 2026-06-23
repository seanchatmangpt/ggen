# Domain-Specific Ontologies

This directory contains four standard domain ontologies downloaded from official sources for use in RDF-based knowledge graphs and semantic web applications.

## Quick Reference

| Ontology | Namespace | Focus | Status |
|----------|-----------|-------|--------|
| **QUDT** | http://qudt.org/2.1/schema/qudt | Quantities, Units, Data Types | Latest (2025) |
| **OWL-TIME** | http://www.w3.org/2006/time | Temporal Concepts & Relations | W3C Standard |
| **GEO** | http://www.w3.org/2003/01/geo/wgs84_pos# | Geographic Coordinates | Stable (2009) |
| **SOSA** | http://www.w3.org/ns/sosa/ | IoT Sensors & Observations | W3C Standard |

## Files

- `qudt-ontology.ttl` (131 KB) - Quantities and Units ontology
- `owl-time.ttl` (100 KB) - Temporal ontology
- `geo.ttl` (7.7 KB) - Geographic positioning vocabulary
- `sosa.ttl` (27 KB) - Sensors and observations vocabulary
- `METADATA.md` - Detailed metadata for each ontology
- `MANIFEST.json` - Machine-readable manifest with version info
- `README.md` - This file

## Usage

### Import in SPARQL/RDF

```turtle
@prefix qudt: <http://qudt.org/schema/qudt/> .
@prefix time: <http://www.w3.org/2006/time#> .
@prefix geo: <http://www.w3.org/2003/01/geo/wgs84_pos#> .
@prefix sosa: <http://www.w3.org/ns/sosa/> .

# Your data here...
```

### Common Integration Patterns

**IoT Sensor Observation with Location and Timestamp:**
```turtle
ex:observation1 a sosa:Observation ;
  sosa:hasSimpleResult "23.5"^^qudt:Temperature ;
  sosa:resultTime [ a time:Instant ; time:inXSDDateTimeStamp "2026-06-23T10:30:00Z"^^xsd:dateTimeStamp ] ;
  sosa:madeBySensor [ geo:lat "40.7128"^^xsd:decimal ; geo:long "-74.0060"^^xsd:decimal ] .
```

**Temperature Measurement with Unit:**
```turtle
ex:temp1 a qudt:Quantity ;
  qudt:hasUnit qudt:Unit_Celsius ;
  qudt:quantityValue "22.5"^^xsd:decimal .
```

## Download Information

- **Download Date:** 2026-06-23
- **Method:** HTTP direct from official sources
- **Total Size:** 264 KB across 4 files
- **Total Triples:** ~5,758 statements

## Verification

All files verified:
- ✓ Downloaded from official sources (qudt.org, w3.org)
- ✓ Valid RDF/Turtle syntax
- ✓ Base URIs and namespaces resolvable
- ✓ Version metadata present and current
- ✓ No proxied or cached versions

## References

- **QUDT:** https://qudt.org/
- **OWL-TIME:** https://www.w3.org/2006/time/
- **GEO:** https://www.w3.org/2003/01/geo/
- **SOSA:** https://www.w3.org/ns/sosa/

## Additional Resources

For richer geographic data, consider:
- **GEOSPARQL:** https://www.w3.org/2015/spatial/ (full GIS ontology)

For sensor metadata and calibration:
- **SOSA Extensions:** https://www.w3.org/TR/vocab-ssn-ext/

For detailed measurement provenance:
- **PROV:** https://www.w3.org/ns/prov# (already in ggen ontologies)

