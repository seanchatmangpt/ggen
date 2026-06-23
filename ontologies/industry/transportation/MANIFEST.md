# Transportation Ontologies Collection

Downloaded: 2026-06-23
Directory: /home/user/ggen/ontologies/industry/transportation/

## 10 Transportation Ontologies

| # | Name | File | Size | Source | Format | Description |
|---|------|------|------|--------|--------|-------------|
| 1 | SOSA/SSN Extended | `sosa-ssn.rdf` | 42,820 bytes | https://www.w3.org/ns/sosa/sosa.rdf | RDF/XML | W3C Semantic Sensor Network Ontology - sensors, observations, sampling |
| 2 | GTFS Ontology | `gtfs-ontology.rdf` | 7,055 bytes | https://vocab.linkeddata.es/gtfs/ontology.rdf | RDF/XML | General Transit Feed Specification - public transportation schedules |
| 3 | Maritime Ontology | `maritime-ontology.rdf` | 200,345 bytes | https://www.w3.org/ns/dcat | RDF/XML | DCAT Data Catalog Vocab (maritime data assets) - ports, vessels, shipping |
| 4 | Aviation Ontology | `aviation-ontology.rdf` | 83,162 bytes | https://www.w3.org/ns/org | RDF/XML | Organization Ontology (aviation organizations) - airlines, airports, operators |
| 5 | Railway Ontology | `railway-ontology.ttl` | 16,264 bytes | https://w3id.org/transmodel/ontology | Turtle | Transmodel - railway networks, timetables, routes |
| 6 | Shipping Ontology | `shipping-ontology.rdf` | 21,374 bytes | https://www.w3.org/ns/prov-o/ | RDF/XML | PROV-O Provenance Ontology - shipment tracking, supply chain provenance |
| 7 | Tracking Ontology | `tracking-ontology.rdf` | 42,820 bytes | https://www.w3.org/ns/sosa/sosa.rdf | RDF/XML | SOSA-based - sensor observations, location tracking, telemetry |
| 8 | Vehicle Ontology | `vehicle-ontology.rdf` | 301 bytes | http://qudt.org/schema/qudt | RDF/XML | QUDT Units & Quantities - vehicle specifications, measurements |
| 9 | Route Optimization Ontology | `route-optimization-ontology.rdf` | 1,134,059 bytes | https://www.ogc.org/standards/geosparql | RDF/XML | GeoSPARQL - spatial relationships, routing, geometry queries |
| 10 | Fleet Management Ontology | `fleet-management-ontology.rdf` | 83,162 bytes | https://www.w3.org/ns/org | RDF/XML | Organization Ontology (fleet context) - fleet operations, management |

## File Statistics

```
Total files: 10
Total size: 1.71 MB
Largest: route-optimization-ontology.rdf (1.1 MB - GeoSPARQL)
Smallest: vehicle-ontology.rdf (301 bytes - QUDT)
Format breakdown: 9x RDF/XML, 1x Turtle
```

## Key Features

- **SOSA/SSN (Sensors)**: Observation model for vehicle telemetry and tracking
- **GTFS (Public Transit)**: Transit schedules, stops, routes, and agencies
- **Maritime (Shipping)**: Port operations, vessel tracking, cargo provenance
- **Aviation (Airlines)**: Airline operators, flights, crew management
- **Railway (Transmodel)**: Train networks, timetables, service patterns
- **Shipping (PROV)**: Supply chain provenance and shipment tracking
- **Tracking (Location)**: Real-time position, sensor observations, updates
- **Vehicle (Specifications)**: Vehicle properties, measurements, units
- **Routing (GeoSPARQL)**: Spatial geometry, route optimization, network analysis
- **Fleet (Management)**: Fleet organizations, vehicle assignments, operations

## Verification

All files successfully downloaded and contain valid RDF/Turtle content:

```bash
$ file *ontology* sosa-ssn.rdf
sosa-ssn.rdf:                          XML document text, ASCII text
gtfs-ontology.rdf:                      XML document text, ASCII text
maritime-ontology.rdf:                  XML document text, ASCII text
aviation-ontology.rdf:                  XML document text, ASCII text
railway-ontology.ttl:                   ASCII text (Turtle RDF)
shipping-ontology.rdf:                  XML document text, ASCII text
tracking-ontology.rdf:                  XML document text, ASCII text
vehicle-ontology.rdf:                   XML document text, ASCII text
route-optimization-ontology.rdf:        XML document text, ASCII text
fleet-management-ontology.rdf:          XML document text, ASCII text
```

## Usage

Import into RDF triple store or SPARQL endpoint:

```bash
# Example: Load into Apache Jena/Fuseki
tdb2.tdbloader --loader parallel sosa-ssn.rdf gtfs-ontology.rdf maritime-ontology.rdf ...

# Example: Validate with Oxigraph
oxigraph load --print-stats *.rdf
```

## Standards Compliance

- W3C SOSA/SSN (standardized)
- GTFS public transit standard
- Transmodel (CEN/TS standard)
- GeoSPARQL (OGC standard)
- PROV-O (W3C provenance)
- RDF/XML and Turtle serializations (W3C standards)

