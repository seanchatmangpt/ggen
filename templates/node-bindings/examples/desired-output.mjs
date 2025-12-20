/**
 * ggen-core - Node.js bindings (ESM)
 * Generated from RDF ontology - DO NOT EDIT MANUALLY
 *
 * @module ggen-core
 * @version 1.0.0
 */

import { loadNative } from './native-loader.mjs';

// Load the native addon
const native = loadNative();

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {string} RdfFormat
 * RDF serialization format
 * Rust: String
 */

/**
 * @typedef {Object} ParseOptions
 * Options for parsing RDF
 * @property {RdfFormat=} format - RDF format (default: "turtle")
 * @property {boolean=} strict - Strict parsing mode
 */

/**
 * @typedef {Object} Triple
 * An RDF triple
 * @property {string} subject - Subject URI
 * @property {string} predicate - Predicate URI
 * @property {string} object - Object value or URI
 */

/**
 * Output format for code generation
 * @typedef {'rust'|'typescript'|'python'|'javascript'} OutputFormat
 */

// =============================================================================
// Exported Functions with JSDoc
// =============================================================================

/**
 * Parse RDF file and return triples
 * @param {string} filePath - Path to RDF file
 * @param {ParseOptions=} options - Parse options
 * @returns {Promise<Triple[]>} Array of RDF triples
 * @throws {Error} If parsing fails
 */
export const parseRdf = native.parseRdf;

/**
 * Generate code from RDF ontology using Tera template
 * @param {string} templatePath - Path to Tera template
 * @param {string} ontologyPath - Path to RDF ontology
 * @param {OutputFormat=} format - Output format
 * @returns {Promise<string>} Generated code
 * @throws {Error} If generation fails
 */
export const generateCode = native.generateCode;

/**
 * Validate RDF ontology against SHACL shapes
 * @param {string} ontologyPath - Path to RDF ontology
 * @param {string=} shapesPath - Path to SHACL shapes
 * @returns {Promise<boolean>} True if valid
 * @throws {Error} If validation fails
 */
export const validateOntology = native.validateOntology;

// =============================================================================
// Default Export
// =============================================================================

/**
 * All exported functions
 * @type {Object}
 */
export default {
  parseRdf,
  generateCode,
  validateOntology,
};
