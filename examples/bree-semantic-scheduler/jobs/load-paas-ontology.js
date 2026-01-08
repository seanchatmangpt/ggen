/**
 * Bree Job: Load ggen PaaS Ontology
 *
 * Preloads and validates the ggen-paas-ontology.ttl into memory.
 * This is a prerequisite job that runs frequently to keep the ontology
 * cache warm for generation pipeline jobs.
 *
 * Generated from: bree-paas-generation.ttl
 * Job Name: LoadPaaSontology
 */

const fs = require('fs');
const path = require('path');
const { Parser, Store } = require('n3');

module.exports = async function load_paas_ontology(job) {
  console.log('[load-paas-ontology] Starting ontology preload...');

  const startTime = Date.now();
  const projectRoot = path.resolve(__dirname, '../../..');

  try {
    // Step 1: Load ontology file
    const ontologyPath = path.join(projectRoot, '.specify/ggen-paas-ontology.ttl');

    if (!fs.existsSync(ontologyPath)) {
      throw new Error(`Ontology file not found: ${ontologyPath}`);
    }

    const turtleData = fs.readFileSync(ontologyPath, 'utf-8');
    console.log(`[load-paas-ontology] Loaded ontology file (${turtleData.length} bytes)`);

    // Step 2: Parse Turtle RDF
    const parser = new Parser({ baseIRI: 'http://ggen.org/paas#' });
    const store = new Store();

    const quads = parser.parse(turtleData);
    console.log(`[load-paas-ontology] Parsed ${quads.length} RDF quads from Turtle`);

    // Step 3: Populate store
    store.addQuads(quads);
    console.log(`[load-paas-ontology] Populated RDF store with ${store.size} triples`);

    // Step 4: Validate ontology structure
    const containers = Array.from(
      store.match(undefined, { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' })
    ).filter(quad =>
      quad.object.value === 'http://ggen.org/paas#Container'
    );

    console.log(`[load-paas-ontology] Found ${containers.length} container definitions`);

    const dataStores = Array.from(
      store.match(undefined, { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' })
    ).filter(quad =>
      quad.object.value === 'http://ggen.org/paas#DataStore'
    );

    console.log(`[load-paas-ontology] Found ${dataStores.length} data store definitions`);

    // Step 5: Validate specification closure
    if (containers.length < 1) {
      throw new Error('No containers defined in ontology (specification incomplete)');
    }

    if (dataStores.length < 3) {
      throw new Error('Insufficient data stores defined (specification incomplete)');
    }

    console.log('[load-paas-ontology] ✓ Specification closure validated');

    // Step 6: Cache statistics
    const stats = {
      containers: containers.length,
      dataStores: dataStores.length,
      totalTriples: store.size,
      timestamp: new Date().toISOString()
    };

    console.log(`[load-paas-ontology] Ontology statistics:`, JSON.stringify(stats, null, 2));

    const duration = Date.now() - startTime;
    console.log(`[load-paas-ontology] ✓ Completed in ${duration}ms`);

    return {
      success: true,
      duration,
      stats,
      cached: true
    };

  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`[load-paas-ontology] ✗ Failed after ${duration}ms: ${error.message}`);
    throw error;
  }
};
