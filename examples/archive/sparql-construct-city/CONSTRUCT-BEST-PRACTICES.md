# 8 Bleeding Edge SPARQL CONSTRUCT Best Practices for Feature Innovation

This example demonstrates 8 advanced SPARQL CONSTRUCT patterns for transforming and enriching RDF graphs with clean, performant, and innovative approaches.

## Overview

These queries transform a simple city ontology into richer derived graphs by:
1. Adding optional properties safely
2. Computing derived attributes
3. Filtering to specific subsets
4. Unioning alternative patterns
5. Aggregating related entities
6. Parameterizing with dynamic values
7. Using graph pattern logic (EXISTS/NOT EXISTS)
8. Following property paths for transitive relationships

---

## 1. CONSTRUCT with OPTIONAL - Safe Property Enrichment

**Pattern**: Add optional properties without requiring them to exist

**Use Case**: Enrich city data with mayor information only when available

```sparql
PREFIX city: <http://example.org/city/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

CONSTRUCT {
  ?city a city:CityProfile ;
    city:name ?name ;
    city:population ?pop ;
    city:currentMayor ?mayorName ;
    city:hasNoMayor true .
}
WHERE {
  ?city a city:City ;
    rdfs:label ?name ;
    city:population ?pop .

  # Optional: Current mayor (the one without endYear or most recent)
  OPTIONAL {
    ?mayor city:mayorsOf ?city ;
      foaf:name ?mayorName ;
      FILTER NOT EXISTS { ?mayor city:endYear ?end }
  }

  # Mark if no mayor found
  BIND(IF(BOUND(?mayorName), "no", "yes") AS ?hasNoMayor)
}
```

**Benefits**:
- Gracefully handles missing data
- Creates default values when patterns don't match
- No null pointers in output
- Discoverable missing relationships

---

## 2. BIND for Computed Values - Derived Properties

**Pattern**: Calculate new properties from existing data

**Use Case**: Compute city statistics (density, size category, founding era)

```sparql
PREFIX city: <http://example.org/city/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

CONSTRUCT {
  ?city a city:CityAnalysis ;
    city:name ?name ;
    city:populationDensity ?density ;
    city:sizeCategory ?sizeCategory ;
    city:foundingEra ?era ;
    city:decimalCentury ?century .
}
WHERE {
  ?city a city:City ;
    rdfs:label ?name ;
    city:population ?pop ;
    city:area ?area ;
    city:founded ?founded .

  # BIND: Compute density (population / area)
  BIND(?pop / ?area AS ?density)

  # BIND: Categorize by population size
  BIND(
    IF(?pop > 1000000, "Major",
      IF(?pop > 100000, "Large",
        IF(?pop > 50000, "Medium", "Small")))
    AS ?sizeCategory
  )

  # BIND: Era from founding year
  BIND(
    IF(?founded < 1700, "Colonial",
      IF(?founded < 1850, "Industrial",
        IF(?founded < 1950, "Modern", "Contemporary")))
    AS ?era
  )

  # BIND: Compute century (for advanced analytics)
  BIND(FLOOR(?founded / 100) * 100 AS ?century)
}
```

**Benefits**:
- Derived data improves query performance downstream
- Encodes business logic at graph level
- Type-safe transformations
- Enables sophisticated analytics

---

## 3. FILTER Expressions - Conditional Graph Generation

**Pattern**: Generate output only for entities matching conditions

**Use Case**: Extract only major cities with significant landmarks

```sparql
PREFIX city: <http://example.org/city/>

CONSTRUCT {
  ?city a city:SignificantCity ;
    city:name ?name ;
    city:population ?pop ;
    city:landmark_count ?landmarkCount ;
    city:neighbors ?neighborCount .
}
WHERE {
  ?city a city:City ;
    rdfs:label ?name ;
    city:population ?pop ;
    city:isMajor true ;
    FILTER (?pop > 500000)

  # Count landmarks
  {
    SELECT ?city (COUNT(?landmark) AS ?landmarkCount)
    WHERE {
      ?landmark city:locatedIn ?city .
    }
    GROUP BY ?city
  }

  # Count neighbors
  {
    SELECT ?city (COUNT(?neighbor) AS ?neighborCount)
    WHERE {
      ?city city:hasNeighbor ?neighbor .
    }
    GROUP BY ?city
  }
}
```

**Benefits**:
- Reduces graph noise by focusing on important entities
- Early filtering improves performance
- Creates focused derived graphs
- Enables multi-level aggregation

---

## 4. UNION for Alternative Patterns - Polymorphic Matching

**Pattern**: Combine multiple graph patterns into single result

**Use Case**: Get all points of interest (landmarks OR events) in a city

```sparql
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?poi a city:PointOfInterest ;
    city:name ?name ;
    city:locatedIn ?city ;
    city:type ?poiType ;
    city:description ?desc .
}
WHERE {
  # Pattern 1: Landmarks
  {
    ?poi a city:Landmark ;
      rdfs:label ?name ;
      city:locatedIn ?city ;
      city:type ?poiType .
    OPTIONAL { ?poi rdfs:comment ?desc }
    BIND("Landmark" AS ?category)
  }
  UNION
  # Pattern 2: Events
  {
    ?poi a city:Event ;
      rdfs:label ?name ;
      city:heldIn ?city .
    BIND("Event" AS ?poiType)
    BIND("Event" AS ?category)
    OPTIONAL { ?poi rdfs:comment ?desc }
  }
}
```

**Benefits**:
- Single query for heterogeneous entity types
- Normalizes disparate schemas
- Enables polymorphic reasoning
- Clean data integration

---

## 5. GROUP_CONCAT Aggregation - Collecting Related Values

**Pattern**: Aggregate multiple related values into single property

**Use Case**: Create city summaries with all neighbors and landmarks

```sparql
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?city a city:CityProfile ;
    city:name ?name ;
    city:allNeighbors ?neighbors ;
    city:allLandmarks ?landmarks ;
    city:neighborList ?neighborList ;
    city:landmarkList ?landmarkList .
}
WHERE {
  ?city a city:City ;
    rdfs:label ?name .

  # GROUP_CONCAT neighbors into pipe-separated list
  {
    SELECT ?city (GROUP_CONCAT(DISTINCT ?neighborName; separator=" | ") AS ?neighborList)
    WHERE {
      ?city city:hasNeighbor ?neighbor .
      ?neighbor rdfs:label ?neighborName .
    }
    GROUP BY ?city
  }

  # GROUP_CONCAT landmarks
  {
    SELECT ?city (GROUP_CONCAT(DISTINCT ?landmarkName; separator=" | ") AS ?landmarkList)
    WHERE {
      ?landmark city:locatedIn ?city .
      ?landmark rdfs:label ?landmarkName .
    }
    GROUP BY ?city
  }

  # Also get raw RDF lists for structured data
  BIND("multiple values aggregated" AS ?neighbors)
  BIND("multiple landmarks aggregated" AS ?landmarks)
}
ORDER BY ?name
```

**Benefits**:
- Summarization without losing detail
- Structured aggregation for reporting
- Performance optimization (fewer triples)
- Human-readable summaries

---

## 6. VALUES for Dynamic Parameters - Parameterized Queries

**Pattern**: Use VALUES clause to provide dynamic parameters

**Use Case**: Generate enriched data for only specific cities (SF Bay Area)

```sparql
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?city a city:BayAreaCity ;
    city:name ?name ;
    city:population ?pop ;
    city:region "San Francisco Bay Area" ;
    city:neighbors ?neighborList ;
    city:landmarks ?landmarks .
}
WHERE {
  # VALUES provides dynamic list of entities to process
  VALUES ?city {
    city:sf
    city:oakland
    city:berkeley
  }

  ?city a city:City ;
    rdfs:label ?name ;
    city:population ?pop .

  # Get neighbors for these specific cities
  {
    SELECT ?city (GROUP_CONCAT(?nname; separator=", ") AS ?neighborList)
    WHERE {
      ?city city:hasNeighbor ?neighbor .
      ?neighbor rdfs:label ?nname .
    }
    GROUP BY ?city
  }

  # Get landmarks for these specific cities
  {
    SELECT ?city (COUNT(?landmark) AS ?landmarks)
    WHERE {
      ?landmark city:locatedIn ?city .
    }
    GROUP BY ?city
  }
}
```

**Benefits**:
- Parameterized query patterns
- Reusable query templates
- Dynamic list processing
- Clean separation of logic and data

---

## 7. FILTER EXISTS / NOT EXISTS - Graph Pattern Logic

**Pattern**: Use existence checks for sophisticated filtering

**Use Case**: Find cities that have landmarks but no mayors, or vice versa

```sparql
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?city a city:CityCharacteristic ;
    city:name ?name ;
    city:hasLandmarks true ;
    city:hasMayors true ;
    city:characterType ?charType ;
    city:populationDensity ?density .
}
WHERE {
  ?city a city:City ;
    rdfs:label ?name ;
    city:population ?pop ;
    city:area ?area .

  BIND(?pop / ?area AS ?density)

  # CHECK: Has landmarks?
  BIND(
    IF(EXISTS {?landmark city:locatedIn ?city}, true, false)
    AS ?hasLandmarks
  )

  # CHECK: Has mayors?
  BIND(
    IF(EXISTS {?mayor city:mayorsOf ?city}, true, false)
    AS ?hasMayors
  )

  # Categorize cities
  BIND(
    IF(?hasLandmarks && ?hasMayors, "Historic and Governed",
      IF(?hasLandmarks, "Historic Unmarked",
        IF(?hasMayors, "Governed but Unmarked", "Anonymous")))
    AS ?charType
  )

  # Only return cities with at least one characteristic
  FILTER(?hasLandmarks || ?hasMayors)
}
```

**Benefits**:
- Sophisticated pattern matching
- Avoids expensive JOINs
- Logical graph patterns
- Clean false/true reasoning

---

## 8. Property Paths - Transitive Relationship Navigation

**Pattern**: Follow paths through the graph without knowing depth

**Use Case**: Find all landmarks reachable through city relationships

```sparql
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?startCity a city:CityWithTransitiveLandmarks ;
    city:name ?startName ;
    city:reachableCity ?reachableName ;
    city:transitiveLandmark ?landmarkName ;
    city:landmarkType ?landmarkType ;
    city:hopDistance ?hops .
}
WHERE {
  ?startCity a city:City ;
    rdfs:label ?startName ;
    city:hasNeighbor+ ?reachableCity .

  ?reachableCity rdfs:label ?reachableName .

  # Find landmarks in reachable cities
  ?landmark city:locatedIn ?reachableCity ;
    rdfs:label ?landmarkName ;
    city:type ?landmarkType .

  # Calculate hop distance (simplified: count neighborhoods)
  {
    SELECT ?startCity ?reachableCity (COUNT(?hop) AS ?hops)
    WHERE {
      ?startCity city:hasNeighbor* ?hop .
      ?hop city:hasNeighbor ?reachableCity .
    }
    GROUP BY ?startCity ?reachableCity
  }
}
ORDER BY ?startName ?hops
```

**Benefits**:
- Graph traversal without recursion
- Discovers paths of unknown depth
- Efficient closure computation
- Enables sophisticated path analytics

---

## Running These Examples

### Prerequisites
```bash
# Install Oxigraph or similar SPARQL endpoint
# These queries are tested with SPARQL 1.1 endpoints
```

### Execute Queries
```bash
# Method 1: Using RDFox or similar
sparql --data data.ttl --query query1-optional.sparql

# Method 2: Using Oxigraph command line
oxigraph query --file data.ttl query1-optional.sparql

# Method 3: Using Python rdflib
python3 << 'EOF'
from rdflib import Graph

g = Graph()
g.parse("data.ttl", format="turtle")

# Execute query
results = g.query("""
[INSERT YOUR CONSTRUCT QUERY HERE]
""")

for row in results:
    print(row)
EOF
```

---

## Key Insights

### Performance Considerations
1. **OPTIONAL is lazy**: Only fetches when needed
2. **BIND is eager**: Computed before filtering
3. **GROUP BY forces aggregation**: Materializes intermediate results
4. **Property paths use algorithms**: Transitive closure computation
5. **FILTER EXISTS minimizes cardinality**: Better than JOIN

### Innovation Patterns
1. **Computed attributes** enable downstream optimization
2. **Aggregation** creates summaries without data loss
3. **EXISTS logic** enables sophisticated reasoning
4. **Property paths** find patterns at any depth
5. **UNION patterns** create flexible schemas

### Graph Design Implications
1. **Enrich at construction time**: Compute once, query many times
2. **Normalize disparate schemas**: UNION for polymorphism
3. **Create summaries**: GROUP_CONCAT for reporting
4. **Build derived graphs**: Materialized views from CONSTRUCT
5. **Enable discovery**: EXISTS patterns surface hidden relationships

---

## Real-World Applications

These patterns enable:
- **Knowledge Graph Enrichment**: Add computed attributes
- **Data Integration**: UNION heterogeneous sources
- **Business Intelligence**: Aggregate and analyze
- **Feature Engineering**: Derive ML training features
- **Data Quality**: FILTER for consistency
- **Reporting**: Create summaries from detail

---

## Further Reading
- [SPARQL 1.1 Query Language Spec](https://www.w3.org/TR/sparql11-query/)
- [Wikidata Query Service Examples](https://query.wikidata.org/)
- [DBpedia Snorkel Tools](https://wiki.dbpedia.org/develop/datasets)
