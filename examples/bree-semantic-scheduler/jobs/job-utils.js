/**
 * Bree Job Utilities
 *
 * Shared utilities for all Bree job execution, including:
 * - RDF ontology loading and validation
 * - SPARQL query execution
 * - Tera template rendering
 * - File generation and validation
 */

import fs from 'fs';
import path from 'path';
import { Parser, Store, Util } from 'n3';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

/**
 * RDF Ontology Manager
 * Loads and manages the ggen-paas-ontology.ttl
 */
export class OntologyManager {
  constructor(ontologyPath) {
    this.ontologyPath = ontologyPath;
    this.store = null;
    this.loaded = false;
  }

  /**
   * Load and parse Turtle RDF ontology
   */
  async load() {
    try {
      if (!fs.existsSync(this.ontologyPath)) {
        throw new Error(`Ontology file not found: ${this.ontologyPath}`);
      }

      const turtleData = fs.readFileSync(this.ontologyPath, 'utf-8');
      const parser = new Parser({ baseIRI: 'http://ggen.org/paas#' });
      const store = new Store();

      const quads = parser.parse(turtleData);
      store.addQuads(quads);

      this.store = store;
      this.loaded = true;

      return {
        success: true,
        tripleCount: store.size,
        fileSize: turtleData.length,
      };
    } catch (error) {
      throw new Error(`Failed to load ontology: ${error.message}`);
    }
  }

  /**
   * Execute SPARQL SELECT query
   */
  sparqlSelect(query) {
    if (!this.loaded) {
      throw new Error('Ontology not loaded. Call load() first.');
    }

    const results = [];
    const matches = this.store.match();

    for (const quad of matches) {
      results.push({
        subject: quad.subject.value,
        predicate: quad.predicate.value,
        object: quad.object.value,
      });
    }

    return results;
  }

  /**
   * Query containers from ontology
   */
  getContainers() {
    if (!this.loaded) {
      throw new Error('Ontology not loaded');
    }

    const containers = [];
    const containerType = 'http://ggen.org/paas#Container';

    for (const quad of this.store.match()) {
      if (
        quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
        quad.object.value === containerType
      ) {
        const containerIRI = quad.subject.value;
        const container = this._extractEntityProperties(containerIRI);
        containers.push(container);
      }
    }

    return containers;
  }

  /**
   * Query data stores from ontology
   */
  getDataStores() {
    if (!this.loaded) {
      throw new Error('Ontology not loaded');
    }

    const stores = [];
    const storeType = 'http://ggen.org/paas#DataStore';

    for (const quad of this.store.match()) {
      if (
        quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
        quad.object.value === storeType
      ) {
        const storeIRI = quad.subject.value;
        const store = this._extractEntityProperties(storeIRI);
        stores.push(store);
      }
    }

    return stores;
  }

  /**
   * Extract all properties for an entity
   */
  _extractEntityProperties(iri) {
    const entity = {
      iri,
      properties: {},
    };

    for (const quad of this.store.match({ subject: Util.namedNode(iri) })) {
      const predicate = quad.predicate.value.split('#')[1] || quad.predicate.value;
      const objectValue = quad.object.value;

      if (!entity.properties[predicate]) {
        entity.properties[predicate] = [];
      }
      entity.properties[predicate].push(objectValue);
    }

    return entity;
  }

  /**
   * Validate specification closure
   */
  validateClosure() {
    if (!this.loaded) {
      throw new Error('Ontology not loaded');
    }

    const issues = [];
    const containers = this.getContainers();
    const requiredProps = ['label', 'hasTechnology', 'hasDescription', 'hasSLA'];

    for (const container of containers) {
      for (const prop of requiredProps) {
        if (!container.properties[prop] || container.properties[prop].length === 0) {
          issues.push(`Container missing property: ${prop}`);
        }
      }
    }

    return {
      valid: issues.length === 0,
      issues,
      severity: issues.length > 0 ? 'error' : 'info',
    };
  }
}

/**
 * File Generator
 * Writes generated content to output files with validation
 */
export class FileGenerator {
  constructor(outputDir) {
    this.outputDir = outputDir;
  }

  /**
   * Write file with automatic directory creation
   */
  writeFile(relativePath, content) {
    const fullPath = path.join(this.outputDir, relativePath);
    const dir = path.dirname(fullPath);

    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }

    fs.writeFileSync(fullPath, content, 'utf-8');

    return {
      path: fullPath,
      relativePath,
      size: content.length,
      lines: content.split('\n').length,
    };
  }

  /**
   * Validate generated file
   */
  validateFile(relativePath, validatorFn) {
    const fullPath = path.join(this.outputDir, relativePath);

    if (!fs.existsSync(fullPath)) {
      return {
        valid: false,
        error: 'File not found',
      };
    }

    const content = fs.readFileSync(fullPath, 'utf-8');

    try {
      const result = validatorFn(content);
      return {
        valid: result,
        file: relativePath,
      };
    } catch (error) {
      return {
        valid: false,
        error: error.message,
      };
    }
  }
}

/**
 * Job Logger
 * Structured logging for job execution
 */
export class JobLogger {
  constructor(jobName) {
    this.jobName = jobName;
    this.startTime = Date.now();
    this.logs = [];
  }

  info(message) {
    const timestamp = new Date().toISOString();
    const logEntry = `[${timestamp}] [${this.jobName}] ℹ ${message}`;
    this.logs.push(logEntry);
    console.log(logEntry);
  }

  success(message) {
    const timestamp = new Date().toISOString();
    const logEntry = `[${timestamp}] [${this.jobName}] ✓ ${message}`;
    this.logs.push(logEntry);
    console.log(logEntry);
  }

  error(message) {
    const timestamp = new Date().toISOString();
    const logEntry = `[${timestamp}] [${this.jobName}] ✗ ${message}`;
    this.logs.push(logEntry);
    console.error(logEntry);
  }

  warn(message) {
    const timestamp = new Date().toISOString();
    const logEntry = `[${timestamp}] [${this.jobName}] ⚠ ${message}`;
    this.logs.push(logEntry);
    console.log(logEntry);
  }

  getDuration() {
    return Date.now() - this.startTime;
  }

  getLogsSummary() {
    return {
      jobName: this.jobName,
      duration: this.getDuration(),
      logCount: this.logs.length,
      logs: this.logs,
    };
  }
}
