/**
 * Query Executor
 * Executes 8 SPARQL CONSTRUCT patterns against city ontology
 */

import { colorize } from './colors.js';

// Query definitions
const QUERIES = {
  optional: {
    title: 'OPTIONAL - Safe Property Enrichment',
    name: 'optional',
    features: ['OPTIONAL', 'BIND', 'FILTER', 'EXISTS'],
    description: 'Enrich cities with optional mayor data without NULLs',
    useCase: 'Add attributes only when available',
    benefits: [
      'Gracefully handles missing data',
      'Creates default values when patterns don\'t match',
      'Discoverable missing relationships',
    ],
    example: `OPTIONAL {
  ?mayor city:mayorsOf ?city ;
    foaf:name ?mayorName ;
    FILTER NOT EXISTS { ?mayor city:endYear ?end }
}`,
    query: `
PREFIX city: <http://example.org/city/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?city a city:CityProfile ;
    city:name ?name ;
    city:population ?pop ;
    city:currentMayor ?mayorName ;
    city:hasMayorInfo ?hasMayorInfo .
}
WHERE {
  ?city a city:City ;
    rdfs:label ?name ;
    city:population ?pop .

  OPTIONAL {
    ?mayor city:mayorsOf ?city ;
      foaf:name ?mayorName ;
      FILTER NOT EXISTS { ?mayor city:endYear ?end }
  }

  BIND(BOUND(?mayorName) AS ?hasMayorInfo)
}
    `,
  },

  bind: {
    title: 'BIND - Computed Values',
    name: 'bind',
    features: ['BIND', 'IF', 'ARITHMETIC'],
    description: 'Compute derived attributes like density and size categories',
    useCase: 'Derive new attributes from existing data',
    benefits: [
      'Type-safe computation',
      'Reduces downstream complexity',
      'Enables optimization (computed once, reused many times)',
    ],
    example: `BIND(?pop / ?area AS ?density)
BIND(
  IF(?pop > 1000000, "Major",
    IF(?pop > 100000, "Large", "Small"))
  AS ?sizeCategory
)`,
    query: `
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?city a city:CityAnalysis ;
    city:name ?name ;
    city:populationDensity ?density ;
    city:sizeCategory ?sizeCategory ;
    city:foundingEra ?era .
}
WHERE {
  ?city a city:City ;
    rdfs:label ?name ;
    city:population ?pop ;
    city:area ?area ;
    city:founded ?founded .

  BIND(?pop / ?area AS ?density)
  BIND(
    IF(?pop > 1000000, "Major",
      IF(?pop > 100000, "Large",
        IF(?pop > 50000, "Medium", "Small")))
    AS ?sizeCategory
  )
  BIND(
    IF(?founded < 1700, "Colonial",
      IF(?founded < 1850, "Industrial",
        IF(?founded < 1950, "Modern", "Contemporary")))
    AS ?era
  )
}
    `,
  },

  filter: {
    title: 'FILTER - Conditional Output',
    name: 'filter',
    features: ['FILTER', 'SELECT', 'GROUP_BY'],
    description: 'Filter major cities with significant landmarks',
    useCase: 'Focus output on important entities',
    benefits: [
      'Reduces graph noise',
      'Better downstream performance',
      'Clear intent (what matters)',
    ],
    example: `WHERE {
  ?city a city:City ;
    city:isMajor true .
  FILTER (?pop > 500000)
}`,
    query: `
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?city a city:SignificantCity ;
    city:name ?name ;
    city:population ?pop ;
    city:landmarkCount ?landmarkCount .
}
WHERE {
  ?city a city:City ;
    rdfs:label ?name ;
    city:population ?pop ;
    city:isMajor true .

  FILTER (?pop > 500000)

  {
    SELECT ?city (COUNT(?landmark) AS ?landmarkCount)
    WHERE {
      ?landmark city:locatedIn ?city .
    }
    GROUP BY ?city
  }
}
    `,
  },

  union: {
    title: 'UNION - Polymorphic Matching',
    name: 'union',
    features: ['UNION', 'BIND'],
    description: 'Get all POIs (landmarks OR events) in a city',
    useCase: 'Unify heterogeneous entity types',
    benefits: [
      'Single query for multiple entity types',
      'Schema flexibility',
      'Clean polymorphism',
    ],
    example: `{
  ?poi a city:Landmark ;
    rdfs:label ?name ;
    city:locatedIn ?city ;
    city:type ?poiType .
}
UNION
{
  ?poi a city:Event ;
    rdfs:label ?name ;
    city:heldIn ?city .
  BIND("Event" AS ?poiType)
}`,
    query: `
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?poi a city:PointOfInterest ;
    city:name ?name ;
    city:locatedIn ?city ;
    city:poiType ?poiType .
}
WHERE {
  {
    ?poi a city:Landmark ;
      rdfs:label ?name ;
      city:locatedIn ?city ;
      city:type ?poiType .
  }
  UNION
  {
    ?poi a city:Event ;
      rdfs:label ?name ;
      city:heldIn ?city .
    BIND("Event" AS ?poiType)
  }
}
    `,
  },

  concat: {
    title: 'GROUP_CONCAT - Aggregation',
    name: 'concat',
    features: ['GROUP_CONCAT', 'SELECT', 'GROUP_BY'],
    description: 'Create city summaries with all neighbors and landmarks',
    useCase: 'Collect related values into summaries',
    benefits: [
      'Summarization without losing detail',
      'Human-readable summaries',
      'Reduces output graph size',
    ],
    example: `SELECT ?city (GROUP_CONCAT(DISTINCT ?neighborName; separator=" | ") AS ?neighborsList)
WHERE {
  ?city city:hasNeighbor ?neighbor .
  ?neighbor rdfs:label ?neighborName .
}
GROUP BY ?city`,
    query: `
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?city a city:CityProfile ;
    city:name ?name ;
    city:neighborsList ?neighborsList ;
    city:landmarksList ?landmarksList .
}
WHERE {
  ?city a city:City ;
    rdfs:label ?name .

  {
    SELECT ?city (GROUP_CONCAT(DISTINCT ?neighborName; separator=" | ") AS ?neighborsList)
    WHERE {
      ?city city:hasNeighbor ?neighbor .
      ?neighbor rdfs:label ?neighborName .
    }
    GROUP BY ?city
  }

  {
    SELECT ?city (GROUP_CONCAT(DISTINCT ?landmarkName; separator=" | ") AS ?landmarksList)
    WHERE {
      ?landmark city:locatedIn ?city .
      ?landmark rdfs:label ?landmarkName .
    }
    GROUP BY ?city
  }
}
    `,
  },

  values: {
    title: 'VALUES - Parameterization',
    name: 'values',
    features: ['VALUES', 'SELECT', 'GROUP_BY'],
    description: 'Process specific cities (Bay Area)',
    useCase: 'Parameterized query processing',
    benefits: [
      'Parameterized query templates',
      'Reusable query templates',
      'Clean separation of logic and data',
    ],
    example: `VALUES ?city {
  city:sf
  city:oakland
  city:berkeley
}`,
    query: `
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?city a city:BayAreaCity ;
    city:name ?name ;
    city:population ?pop ;
    city:region "Bay Area" ;
    city:neighborCount ?neighborCount .
}
WHERE {
  VALUES ?city {
    city:sf
    city:oakland
    city:berkeley
  }

  ?city a city:City ;
    rdfs:label ?name ;
    city:population ?pop .

  {
    SELECT ?city (COUNT(?neighbor) AS ?neighborCount)
    WHERE {
      ?city city:hasNeighbor ?neighbor .
    }
    GROUP BY ?city
  }
}
    `,
  },

  exists: {
    title: 'EXISTS / NOT EXISTS - Graph Logic',
    name: 'exists',
    features: ['EXISTS', 'BIND', 'FILTER'],
    description: 'Find cities with/without landmarks or mayors',
    useCase: 'Sophisticated pattern matching',
    benefits: [
      'Sophisticated logical patterns',
      'Avoids expensive JOINs',
      'Clear intent',
    ],
    example: `BIND(
  IF(EXISTS {?landmark city:locatedIn ?city}, true, false)
  AS ?hasLandmarks
)`,
    query: `
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?city a city:CityCharacteristic ;
    city:name ?name ;
    city:hasLandmarks ?hasLandmarks ;
    city:hasMayors ?hasMayors ;
    city:characterType ?characterType .
}
WHERE {
  ?city a city:City ;
    rdfs:label ?name .

  BIND(
    IF(EXISTS {?landmark city:locatedIn ?city}, true, false)
    AS ?hasLandmarks
  )

  BIND(
    IF(EXISTS {?mayor city:mayorsOf ?city}, true, false)
    AS ?hasMayors
  )

  BIND(
    IF(?hasLandmarks && ?hasMayors, "Historic",
      IF(?hasLandmarks, "Unmapped",
        IF(?hasMayors, "Unlandmarked", "Unknown")))
    AS ?characterType
  )
}
    `,
  },

  paths: {
    title: 'Property Paths - Transitive Navigation',
    name: 'paths',
    features: ['PROPERTY PATHS', '+', '*'],
    description: 'Find landmarks reachable through city relationships',
    useCase: 'Follow paths at any depth',
    benefits: [
      'Graph traversal without recursion',
      'Closure computation',
      'Path finding',
    ],
    example: `WHERE {
  ?startCity a city:City ;
    rdfs:label ?startName ;
    city:hasNeighbor+ ?reachableCity .
}`,
    query: `
PREFIX city: <http://example.org/city/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

CONSTRUCT {
  ?startCity a city:CityWithTransitiveLandmarks ;
    city:name ?startName ;
    city:reachableLandmark ?landmarkName ;
    city:landmarkType ?landmarkType .
}
WHERE {
  ?startCity a city:City ;
    rdfs:label ?startName ;
    city:hasNeighbor+ ?reachableCity .

  ?landmark city:locatedIn ?reachableCity ;
    rdfs:label ?landmarkName ;
    city:type ?landmarkType .
}
    `,
  },
};

/**
 * Execute a SPARQL CONSTRUCT query
 * For now, returns mock results (would integrate with Oxigraph in production)
 */
export async function executeQuery(pattern, options = {}) {
  const queryDef = QUERIES[pattern];

  if (!queryDef) {
    return {
      success: false,
      error: `Unknown pattern: ${pattern}. Use 'ggen-sparql list' to see available patterns.`,
    };
  }

  try {
    // Mock execution - in production would run against Oxigraph
    // For now, return synthetic results to demonstrate CLI functionality

    const results = {
      optional: {
        tripleCount: 5,
        data: `@prefix city: <http://example.org/city/> .

city:sf a city:CityProfile ;
  city:name "San Francisco" ;
  city:population 873965 ;
  city:currentMayor "London Breed" ;
  city:hasMayorInfo true .

city:oakland a city:CityProfile ;
  city:name "Oakland" ;
  city:population 433031 ;
  city:hasMayorInfo false .`,
      },
      bind: {
        tripleCount: 5,
        data: `@prefix city: <http://example.org/city/> .

city:sf a city:CityAnalysis ;
  city:name "San Francisco" ;
  city:populationDensity 18595.0 ;
  city:sizeCategory "Major" ;
  city:foundingEra "Colonial" .

city:oakland a city:CityAnalysis ;
  city:name "Oakland" ;
  city:populationDensity 7753.6 ;
  city:sizeCategory "Large" ;
  city:foundingEra "Industrial" .`,
      },
      filter: {
        tripleCount: 2,
        data: `@prefix city: <http://example.org/city/> .

city:sf a city:SignificantCity ;
  city:name "San Francisco" ;
  city:population 873965 ;
  city:landmarkCount 3 .

city:nyc a city:SignificantCity ;
  city:name "New York City" ;
  city:population 8335897 ;
  city:landmarkCount 1 .`,
      },
      union: {
        tripleCount: 4,
        data: `@prefix city: <http://example.org/city/> .

city:GoldenGate a city:PointOfInterest ;
  city:name "Golden Gate Bridge" ;
  city:locatedIn city:sf ;
  city:poiType "Bridge" .

city:SF_Tech_Summit a city:PointOfInterest ;
  city:name "SF Tech Summit" ;
  city:locatedIn city:sf ;
  city:poiType "Event" .`,
      },
      concat: {
        tripleCount: 3,
        data: `@prefix city: <http://example.org/city/> .

city:sf a city:CityProfile ;
  city:name "San Francisco" ;
  city:neighborsList "Oakland | Berkeley" ;
  city:landmarksList "Golden Gate Bridge | Painted Ladies | Alcatraz" .

city:oakland a city:CityProfile ;
  city:name "Oakland" ;
  city:neighborsList "San Francisco | Berkeley" ;
  city:landmarksList "" .`,
      },
      values: {
        tripleCount: 3,
        data: `@prefix city: <http://example.org/city/> .

city:sf a city:BayAreaCity ;
  city:name "San Francisco" ;
  city:population 873965 ;
  city:region "Bay Area" ;
  city:neighborCount 2 .

city:oakland a city:BayAreaCity ;
  city:name "Oakland" ;
  city:population 433031 ;
  city:region "Bay Area" ;
  city:neighborCount 2 .`,
      },
      exists: {
        tripleCount: 5,
        data: `@prefix city: <http://example.org/city/> .

city:sf a city:CityCharacteristic ;
  city:name "San Francisco" ;
  city:hasLandmarks true ;
  city:hasMayors true ;
  city:characterType "Historic" .

city:nyc a city:CityCharacteristic ;
  city:name "New York City" ;
  city:hasLandmarks true ;
  city:hasMayors true ;
  city:characterType "Historic" .`,
      },
      paths: {
        tripleCount: 4,
        data: `@prefix city: <http://example.org/city/> .

city:sf a city:CityWithTransitiveLandmarks ;
  city:name "San Francisco" ;
  city:reachableLandmark "Lawrence Berkeley Laboratory" ;
  city:landmarkType "Research Institute" ;
  city:reachableLandmark "Golden Gate Bridge" ;
  city:landmarkType "Bridge" .`,
      },
    };

    const result = results[pattern] || { tripleCount: 0, data: '# No results' };

    if (options.format === 'json') {
      return {
        success: true,
        tripleCount: result.tripleCount,
        data: {
          pattern,
          tripleCount: result.tripleCount,
          query: queryDef.query.trim(),
        },
      };
    }

    return {
      success: true,
      tripleCount: result.tripleCount,
      data: result.data,
    };
  } catch (err) {
    return {
      success: false,
      error: err.message,
    };
  }
}

/**
 * Get detailed information about a query pattern
 */
export function getQueryInfo(pattern) {
  return QUERIES[pattern] || null;
}

/**
 * List all available query patterns
 */
export function listQueries() {
  return Object.values(QUERIES);
}
