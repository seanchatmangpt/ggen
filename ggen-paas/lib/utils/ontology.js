/**
 * ggen-paas CLI - OntologyManager Utility
 *
 * Loads and queries the ggen-paas RDF ontology.
 * Provides access to containers, data stores, and other infrastructure components.
 */

import fs from 'fs';
import path from 'path';

export class OntologyManager {
  constructor(ontologyPath = null) {
    if (!ontologyPath) {
      // Default path: look for .specify/ggen-paas-ontology.ttl
      const projectRoot = process.cwd();
      ontologyPath = path.join(projectRoot, '.specify/ggen-paas-ontology.ttl');
    }

    this.ontologyPath = ontologyPath;
    this.store = null;
    this.util = null;
    this.loaded = false;
  }

  /**
   * Load the RDF ontology from disk
   */
  async load() {
    if (this.loaded) {
      return { success: true, cached: true, tripleCount: this.store.size };
    }

    try {
      // Lazy-load n3 library
      const n3 = await import('n3');
      const { Parser, Store, Util } = n3;

      if (!fs.existsSync(this.ontologyPath)) {
        throw new Error(`Ontology file not found: ${this.ontologyPath}`);
      }

      const turtleData = fs.readFileSync(this.ontologyPath, 'utf-8');
      const parser = new Parser({ baseIRI: 'http://ggen.org/paas#' });
      const store = new Store();

      const quads = parser.parse(turtleData);
      store.addQuads(quads);

      this.store = store;
      this.util = Util;
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
   * Get all containers from the ontology
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
   * Get all data stores from the ontology
   */
  getDataStores() {
    if (!this.loaded) {
      throw new Error('Ontology not loaded');
    }

    const dataStores = [];
    const dataStoreType = 'http://ggen.org/paas#DataStore';

    for (const quad of this.store.match()) {
      if (
        quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
        quad.object.value === dataStoreType
      ) {
        const dataStoreIRI = quad.subject.value;
        const dataStore = this._extractEntityProperties(dataStoreIRI);
        dataStores.push(dataStore);
      }
    }

    return dataStores;
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
    const dataStores = this.getDataStores();

    // Check containers have required properties
    const requiredContainerProps = ['label', 'hasTechnology', 'hasDescription', 'hasSLA'];
    for (const container of containers) {
      for (const prop of requiredContainerProps) {
        if (!container.properties[prop] || container.properties[prop].length === 0) {
          issues.push(`Container ${container.iri.split('#')[1]} missing property: ${prop}`);
        }
      }
    }

    // Check data stores have required properties
    const requiredDataStoreProps = ['label', 'hasTechnology', 'hasDescription'];
    for (const ds of dataStores) {
      for (const prop of requiredDataStoreProps) {
        if (!ds.properties[prop] || ds.properties[prop].length === 0) {
          issues.push(`DataStore ${ds.iri.split('#')[1]} missing property: ${prop}`);
        }
      }
    }

    return {
      valid: issues.length === 0,
      issues,
      stats: {
        containers: containers.length,
        dataStores: dataStores.length,
        containerIssues: issues.filter(i => i.includes('Container')).length,
        dataStoreIssues: issues.filter(i => i.includes('DataStore')).length,
      },
    };
  }

  /**
   * Extract all properties for an entity
   * @private
   */
  _extractEntityProperties(iri) {
    const entity = {
      iri,
      properties: {},
    };

    for (const quad of this.store.match({ subject: this.util.namedNode(iri) })) {
      const predicate = quad.predicate.value.split('#')[1] || quad.predicate.value;
      const objectValue = quad.object.value;

      if (!entity.properties[predicate]) {
        entity.properties[predicate] = [];
      }
      entity.properties[predicate].push(objectValue);
    }

    return entity;
  }
}
