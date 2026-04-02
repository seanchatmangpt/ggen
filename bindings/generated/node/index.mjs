/**
 * ggen-bindings - Node.js bindings (ESM)
 * Generated from RDF ontology using 3T approach (TOML, Tera, Turtle)
 *
 * DO NOT EDIT MANUALLY - Regenerate using: ggen sync
 *
 * Source: ../../../schema/ggen-ffi-api.ttl
 * Version: 5.0.1
 */

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

// Load native addon (assumes Neon/N-API addon built separately)
let native;
try {
  native = require('../../native/index.node');
} catch (err) {
  throw new Error(`Failed to load native ggen addon: ${err.message}\nDid you build the Rust bindings with Neon?`);
}

// ============================================================================
// Type Definitions (JSDoc for IDE support)
// ============================================================================

/**
 * Supported RDF serialization formats
 * @typedef {'turtle' | 'ntriples' | 'rdfxml' | 'jsonld'} RdfFormat
 */

/**
 * An RDF triple (subject, predicate, object)
 * @typedef {Object} Triple
 * @property {string} subject - Subject URI or blank node
 * @property {string} predicate - Predicate URI
 * @property {string} object - Object value (URI, literal, or blank node)
 */

/**
 * Configuration for code generation
 * @typedef {Object} GenerationConfig
 * @property {string} templatePath - Path to Tera template file
 * @property {string} ontologyPath - Path to RDF ontology file
 * @property {string} [outputPath] - Output file path (optional, defaults to stdout)
 * @property {boolean} [verbose=false] - Enable verbose logging
 */

/**
 * Result of ontology validation
 * @typedef {Object} ValidationResult
 * @property {boolean} valid - Whether the ontology is valid
 * @property {string[]} errors - Validation error messages
 * @property {string[]} warnings - Validation warning messages
 */

// ============================================================================
// Function: parseRdf
// ============================================================================

/**
 * Parse an RDF file and return triples
 *
 * This function parses an RDF file in various formats and returns an array of triples.
 * The format is auto-detected if not specified.
 *
 * @param {string} filePath - Path to RDF file to parse
 * @param {RdfFormat} [format='turtle'] - RDF serialization format (defaults to Turtle)
 * @returns {Promise<Triple[]>} Array of parsed triples
 * @throws {Error} If parsing fails
 *
 * @example
 * // Parse a Turtle file
 * const triples = await parseRdf('./ontology.ttl');
 * console.log(`Parsed ${triples.length} triples`);
 *
 * @example
 * // Parse JSON-LD explicitly
 * const triples = await parseRdf('./data.jsonld', 'jsonld');
 * triples.forEach(t => console.log(`${t.subject} ${t.predicate} ${t.object}`));
 */
export const parseRdf = native.parseRdf;

// ============================================================================
// Function: generateCode
// ============================================================================

/**
 * Generate code from RDF ontology using Tera template
 *
 * This function takes an ontology and a Tera template, executes SPARQL queries
 * embedded in the template, and generates code by rendering the template with
 * query results.
 *
 * @param {GenerationConfig} config - Code generation configuration
 * @returns {Promise<string>} Generated code as string
 * @throws {Error} If generation fails
 *
 * @example
 * // Generate TypeScript types from ontology
 * const code = await generateCode({
 *   templatePath: './templates/types.ts.tera',
 *   ontologyPath: './schema/api.ttl',
 *   outputPath: './generated/types.ts',
 *   verbose: true
 * });
 * console.log('Generated:', code);
 *
 * @example
 * // Generate to stdout (no outputPath)
 * const result = await generateCode({
 *   templatePath: './template.tera',
 *   ontologyPath: './ontology.ttl'
 * });
 * console.log(result);
 */
export const generateCode = native.generateCode;

// ============================================================================
// Function: validateOntology
// ============================================================================

/**
 * Validate RDF ontology for consistency and correctness
 *
 * Performs SHACL validation, checks for common RDF patterns, and reports
 * errors and warnings.
 *
 * @param {string} ontologyPath - Path to RDF ontology file to validate
 * @returns {Promise<ValidationResult>} Validation result with errors/warnings
 * @throws {Error} If validation process fails (not if ontology is invalid)
 *
 * @example
 * // Validate an ontology
 * const result = await validateOntology('./schema/api.ttl');
 * if (result.valid) {
 *   console.log('✅ Ontology is valid!');
 * } else {
 *   console.error('❌ Validation failed:');
 *   result.errors.forEach(err => console.error(`  - ${err}`));
 * }
 * if (result.warnings.length > 0) {
 *   console.warn('⚠️  Warnings:');
 *   result.warnings.forEach(warn => console.warn(`  - ${warn}`));
 * }
 */
export const validateOntology = native.validateOntology;

// ============================================================================
// Function: executeSparql
// ============================================================================

/**
 * Execute SPARQL query against RDF ontology
 *
 * Runs a SPARQL SELECT or CONSTRUCT query against the specified ontology
 * and returns results as JSON.
 *
 * @param {string} ontologyPath - Path to RDF ontology file
 * @param {string} query - SPARQL query string
 * @returns {Promise<any>} Query results (format depends on query type)
 * @throws {Error} If query execution fails
 *
 * @example
 * // Execute SPARQL SELECT query
 * const query = `
 *   PREFIX ffi: <https://ggen.dev/ontology/node-ffi#>
 *   SELECT ?func ?name WHERE {
 *     ?func a ffi:Function ;
 *           ffi:functionName ?name .
 *   }
 * `;
 * const results = await executeSparql('./schema/api.ttl', query);
 * console.log('Functions:', results);
 *
 * @example
 * // Execute SPARQL CONSTRUCT query
 * const construct = `
 *   PREFIX ffi: <https://ggen.dev/ontology/node-ffi#>
 *   CONSTRUCT { ?s ?p ?o }
 *   WHERE { ?s a ffi:Function . ?s ?p ?o . }
 * `;
 * const graph = await executeSparql('./schema/api.ttl', construct);
 */
export const executeSparql = native.executeSparql;

// ============================================================================
// Module Metadata
// ============================================================================

/**
 * Module name
 * @type {string}
 */
export const MODULE_NAME = 'ggen-bindings';

/**
 * Module version
 * @type {string}
 */
export const MODULE_VERSION = '5.0.1';

/**
 * Module description
 * @type {string}
 */
export const MODULE_DESCRIPTION = 'Multi-language bindings for ggen code generator';
