# Domain-Specific Ontologies - Download Report

Downloaded: 2026-06-23

## 1. QUDT (Quantities, Units, Dimension and Data Types)

| Property | Value |
|----------|-------|
| **Filename** | `qudt-ontology.ttl` |
| **Source URL** | https://qudt.org/2.1/schema/qudt |
| **Version** | 2.1 (Latest) |
| **Modified** | 2025-01-28T18:38:46Z |
| **Size** | 134,067 bytes |
| **Lines** | 3,357 |
| **Format** | Turtle (TTL) |
| **Base URI** | http://qudt.org/2.1/schema/qudt |
| **Status** | ✓ Successfully downloaded |

**Description:** QUDT is an ontology for representing quantities (physical, chemical, technological properties), units of measure, and data types for measurements. It provides standardized definitions for SI units and quantities.

**Key Classes:** Quantity, Unit, QuantityKind, Dimension, DatatypeDefinition

---

## 2. OWL-TIME (W3C Temporal Ontology)

| Property | Value |
|----------|-------|
| **Filename** | `owl-time.ttl` |
| **Source URL** | https://www.w3.org/2006/time |
| **Version** | W3C standard (2006) |
| **Modified** | 2017-04-06 |
| **Version IRI** | http://www.w3.org/2006/time#2016 |
| **Size** | 101,486 bytes |
| **Lines** | 1,785 |
| **Format** | Turtle (TTL) |
| **Base URI** | http://www.w3.org/2006/time |
| **Status** | ✓ Successfully downloaded |

**Description:** OWL-TIME is a W3C standard ontology for representing temporal concepts and relations. It defines temporal entities (instants, intervals), temporal reference systems, and temporal relations (before, after, during, etc.).

**Key Classes:** TemporalEntity, Instant, Interval, ProperInterval, TemporalReference, DateTimeInterval

---

## 3. GEO (WGS84 Geo Positioning)

| Property | Value |
|----------|-------|
| **Filename** | `geo.ttl` |
| **Source URL** | https://www.w3.org/2003/01/geo/wgs84_pos |
| **Version** | 1.22 (legacy, stable) |
| **Modified** | 2009-04-20 |
| **Size** | 7,866 bytes |
| **Lines** | 192 |
| **Format** | RDF/XML (served as text, converted to TTL) |
| **Base URI** | http://www.w3.org/2003/01/geo/wgs84_pos# |
| **Status** | ✓ Successfully downloaded |

**Description:** WGS84 Geo is a lightweight vocabulary for representing geographic coordinates (latitude, longitude, altitude) in the WGS84 geodetic reference datum. It is widely used for geo-tagging and location annotation.

**Key Properties:** lat, long, alt (latitude, longitude, altitude)

**Note:** This is a minimal but stable vocabulary. For richer geographic data, consider GEOSPARQL in future enhancements.

---

## 4. SOSA (Sensors, Observations, Samples, Actuators)

| Property | Value |
|----------|-------|
| **Filename** | `sosa.ttl` |
| **Source URL** | https://www.w3.org/ns/sosa/ |
| **Version** | W3C standard |
| **Created** | 2017-04-17 |
| **Size** | 27,326 bytes |
| **Lines** | 424 |
| **Format** | Turtle (TTL) |
| **Base URI** | http://www.w3.org/ns/sosa/ |
| **Status** | ✓ Successfully downloaded |

**Description:** SOSA is a W3C Semantic Web standard for the Internet of Things (IoT). It provides a unified vocabulary for representing sensors, observations, actuators, and samplers. Often used with QUDT for sensor measurement units and with OWL-TIME for temporal aspects of observations.

**Key Classes:** Sensor, Actuator, Sampler, Observation, Actuation, Sampling, ObservableProperty, Sample

**Common Integration:** SOSA + QUDT + OWL-TIME for complete sensor-observation-measurement workflows

---

## Download Summary

| Ontology | File | Size | Lines | Source |
|----------|------|------|-------|--------|
| QUDT | qudt-ontology.ttl | 134 KB | 3,357 | https://qudt.org |
| OWL-TIME | owl-time.ttl | 101 KB | 1,785 | https://w3.org |
| GEO | geo.ttl | 8 KB | 192 | https://w3.org |
| SOSA | sosa.ttl | 27 KB | 424 | https://w3.org |
| **TOTAL** | 4 files | **270 KB** | **5,758** | Official sources |

---

## Integration Notes

### Typical Usage Patterns

1. **IoT Sensor Networks (SOSA + QUDT + OWL-TIME)**
   - SOSA for sensor/observation structure
   - QUDT for measurement units and dimensions
   - OWL-TIME for temporal timestamps and intervals

2. **Geographic Data (GEO + QUDT)**
   - GEO for lat/long/alt coordinates
   - QUDT for elevation units and precision

3. **Scientific Measurements (QUDT + OWL-TIME)**
   - QUDT for quantities and units
   - OWL-TIME for measurement timestamps and valid time periods

4. **Complete IoT Pipeline (SOSA + QUDT + OWL-TIME + GEO)**
   - Sensor produces observation
   - Observation has value (QUDT unit)
   - Observation has timestamp (OWL-TIME)
   - Observation has location (GEO)

---

## Verification

All downloads verified:
- ✓ Files exist and are non-empty
- ✓ RDF/TTL syntax valid (parseable by any RDF parser)
- ✓ Base URIs and namespaces resolvable
- ✓ Version metadata extracted
- ✓ Real downloads from official sources (not cached/proxy)

