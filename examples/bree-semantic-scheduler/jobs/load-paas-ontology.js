/**
 * Bree Job: Load PaaS Ontology
 *
 * Preloads and validates the ggen-paas-ontology.ttl into memory.
 * This is a prerequisite job that runs frequently to keep the ontology
 * cache warm for generation pipeline jobs.
 *
 * Generated from: bree-paas-generation.ttl
 * Job Name: LoadPaaSontology
 */

import path from 'path';
import { fileURLToPath } from 'url';
import { OntologyManager, JobLogger } from './job-utils.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default async function load_paas_ontology(job) {
  const logger = new JobLogger('load-paas-ontology');
  const projectRoot = path.resolve(__dirname, '../../..');

  try {
    logger.info('Starting ontology preload...');

    // Step 1: Initialize ontology manager
    const ontologyPath = path.join(projectRoot, '.specify/ggen-paas-ontology.ttl');
    const ontologyMgr = new OntologyManager(ontologyPath);

    logger.info(`Loading ontology from: ${ontologyPath}`);

    // Step 2: Load and parse ontology
    const loadResult = await ontologyMgr.load();
    logger.success(
      `Ontology loaded: ${loadResult.tripleCount} triples, ${loadResult.fileSize} bytes`
    );

    // Step 3: Extract containers and data stores
    const containers = ontologyMgr.getContainers();
    logger.info(`Found ${containers.length} container definitions`);

    const dataStores = ontologyMgr.getDataStores();
    logger.info(`Found ${dataStores.length} data store definitions`);

    // Step 4: Validate specification closure
    const closureValidation = ontologyMgr.validateClosure();

    if (!closureValidation.valid) {
      logger.warn(`Specification closure issues found:`);
      closureValidation.issues.forEach((issue) => logger.warn(`  - ${issue}`));
    } else {
      logger.success('Specification closure validated');
    }

    // Step 5: Build cache statistics
    const stats = {
      timestamp: new Date().toISOString(),
      ontologyPath,
      tripleCount: loadResult.tripleCount,
      containers: containers.length,
      dataStores: dataStores.length,
      closureValid: closureValidation.valid,
      closureIssues: closureValidation.issues.length,
      duration: logger.getDuration(),
    };

    logger.success(`Ontology preload complete in ${logger.getDuration()}ms`);

    return {
      success: true,
      ...stats,
      logs: logger.getLogsSummary(),
    };
  } catch (error) {
    logger.error(`Failed: ${error.message}`);
    throw error;
  }
}
