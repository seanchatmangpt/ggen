/**
 * Production-grade Node.js N-API bindings for ggen CLI
 *
 * This module provides high-performance, type-safe bindings to the ggen CLI
 * for use in Node.js and browser environments via WASM.
 *
 * @packageDocumentation
 */

/* eslint-disable @typescript-eslint/no-var-requires */
const addon = require('./index.node');

/**
 * @typedef {Object} RunResult
 * @property {number} code - Exit code from CLI execution
 * @property {string} stdout - Standard output from CLI command
 * @property {string} stderr - Standard error output from CLI command
 */

/**
 * Get the ggen version (matches crate version)
 *
 * @returns {string} Semantic version string (e.g., "3.4.0")
 *
 * @example
 * ```javascript
 * import { version } from '@ggen/node';
 * console.log('ggen version:', version());
 * ```
 */
export function version() {
  return addon.version();
}

/**
 * Low-level entrypoint to invoke the CLI.
 * Prefer higher-level wrapper functions for better ergonomics.
 *
 * @param {string[]} args - Command-line arguments (without the 'ggen' binary name)
 * @returns {Promise<RunResult>} Result containing exit code and captured stdout/stderr
 *
 * @example
 * ```javascript
 * const result = await run(['--version']);
 * console.log(result.stdout);
 * ```
 */
export async function run(args) {
  return addon.run(args);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Marketplace Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Search for marketplace packages by keyword
 *
 * @param {string} query - Search query (e.g., "rust", "typescript", "template")
 * @returns {Promise<RunResult>} Search results in stdout
 *
 * @example
 * ```javascript
 * const result = await marketSearch('rust-templates');
 * console.log(result.stdout);
 * ```
 */
export async function marketSearch(query) {
  return addon.marketSearch(query);
}

/**
 * List all available marketplace packages
 *
 * @returns {Promise<RunResult>} List of packages in stdout
 *
 * @example
 * ```javascript
 * const result = await marketList();
 * console.log(result.stdout);
 * ```
 */
export async function marketList() {
  return addon.marketList();
}

/**
 * Get marketplace package categories
 *
 * @returns {Promise<RunResult>} Category list in stdout
 *
 * @example
 * ```javascript
 * const result = await marketCategories();
 * console.log(result.stdout);
 * ```
 */
export async function marketCategories() {
  return addon.marketCategories();
}

/**
 * Add a marketplace package to local cache
 *
 * @param {string} packageId - Package identifier (e.g., "rust-rest-api")
 * @returns {Promise<RunResult>} Installation result
 *
 * @example
 * ```javascript
 * const result = await marketAdd('rust-rest-api');
 * console.log(result.stdout);
 * ```
 */
export async function marketAdd(packageId) {
  return addon.marketAdd(packageId);
}

/**
 * Remove a marketplace package from local cache
 *
 * @param {string} packageId - Package identifier
 * @returns {Promise<RunResult>} Removal result
 *
 * @example
 * ```javascript
 * const result = await marketRemove('rust-rest-api');
 * console.log(result.stdout);
 * ```
 */
export async function marketRemove(packageId) {
  return addon.marketRemove(packageId);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Template Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * List all available templates
 *
 * @returns {Promise<RunResult>} Template list in stdout
 *
 * @example
 * ```javascript
 * const result = await templateList();
 * console.log(result.stdout);
 * ```
 */
export async function templateList() {
  return addon.templateList();
}

/**
 * Show template details and structure
 *
 * @param {string} templatePath - Path to template file or directory
 * @returns {Promise<RunResult>} Template details in stdout
 *
 * @example
 * ```javascript
 * const result = await templateShow('./my-template.tmpl');
 * console.log(result.stdout);
 * ```
 */
export async function templateShow(templatePath) {
  return addon.templateShow(templatePath);
}

/**
 * Lint a template for syntax and semantic errors
 *
 * @param {string} templatePath - Path to template file
 * @returns {Promise<RunResult>} Linting results
 *
 * @example
 * ```javascript
 * const result = await templateLint('./my-template.tmpl');
 * if (result.code !== 0) {
 *   console.error('Linting errors:', result.stderr);
 * }
 * ```
 */
export async function templateLint(templatePath) {
  return addon.templateLint(templatePath);
}

/**
 * Generate code from a template
 *
 * @param {string} templatePath - Path to template file
 * @param {string} outputPath - Path to write generated output
 * @param {Object} [context] - Template context variables (optional)
 * @returns {Promise<RunResult>} Generation result
 *
 * @example
 * ```javascript
 * const result = await templateGenerate(
 *   './my-template.tmpl',
 *   './output.rs',
 *   { className: 'User', fields: ['id', 'name'] }
 * );
 * ```
 */
export async function templateGenerate(templatePath, outputPath, context) {
  return addon.templateGenerate(templatePath, outputPath, context);
}

/**
 * Create a new template from scratch
 *
 * @param {string} templateName - Name for the new template
 * @param {string} outputPath - Path to write template file
 * @param {string} [language] - Programming language (e.g., "rust", "typescript")
 * @returns {Promise<RunResult>} Template creation result
 *
 * @example
 * ```javascript
 * const result = await templateNew('my-template', './templates/my-template.tmpl', 'rust');
 * console.log(result.stdout);
 * ```
 */
export async function templateNew(templateName, outputPath, language) {
  return addon.templateNew(templateName, outputPath, language);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Project Scaffolding Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Create a new project from template or ontology
 *
 * @param {string} projectName - Name for the new project
 * @param {string} [templateOrOntology] - Template name or ontology path
 * @param {string} [language] - Programming language (e.g., "rust", "typescript", "python")
 * @returns {Promise<RunResult>} Project creation result
 *
 * @example
 * ```javascript
 * const result = await projectNew('my-api', 'rest-api', 'rust');
 * console.log(result.stdout);
 * ```
 */
export async function projectNew(projectName, templateOrOntology, language) {
  return addon.projectNew(projectName, templateOrOntology, language);
}

/**
 * Generate code for an existing project
 *
 * @param {string} [projectPath] - Path to project (defaults to current directory)
 * @returns {Promise<RunResult>} Generation result
 *
 * @example
 * ```javascript
 * const result = await projectGenerate('./my-project');
 * console.log(result.stdout);
 * ```
 */
export async function projectGenerate(projectPath) {
  return addon.projectGenerate(projectPath);
}

/**
 * Watch project for changes and auto-regenerate code
 *
 * @param {string} [projectPath] - Path to project (defaults to current directory)
 * @returns {Promise<RunResult>} Watch mode result
 *
 * @example
 * ```javascript
 * // Runs until interrupted
 * const result = await projectWatch('./my-project');
 * ```
 */
export async function projectWatch(projectPath) {
  return addon.projectWatch(projectPath);
}

/**
 * Initialize project with ggen configuration
 *
 * @param {string} [projectPath] - Path to project (defaults to current directory)
 * @returns {Promise<RunResult>} Initialization result
 *
 * @example
 * ```javascript
 * const result = await projectInit('./my-project');
 * console.log(result.stdout);
 * ```
 */
export async function projectInit(projectPath) {
  return addon.projectInit(projectPath);
}

/**
 * Plan code generation for a project
 *
 * @param {string} [projectPath] - Path to project (defaults to current directory)
 * @returns {Promise<RunResult>} Planning result with what would be generated
 *
 * @example
 * ```javascript
 * const result = await projectPlan('./my-project');
 * console.log(result.stdout); // Shows proposed changes
 * ```
 */
export async function projectPlan(projectPath) {
  return addon.projectPlan(projectPath);
}

/**
 * Apply previously planned changes
 *
 * @param {string} [projectPath] - Path to project (defaults to current directory)
 * @returns {Promise<RunResult>} Application result
 *
 * @example
 * ```javascript
 * const result = await projectApply('./my-project');
 * console.log(result.stdout);
 * ```
 */
export async function projectApply(projectPath) {
  return addon.projectApply(projectPath);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Lifecycle Management Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * List available lifecycle phases
 *
 * @returns {Promise<RunResult>} Phase list in stdout
 *
 * @example
 * ```javascript
 * const result = await lifecycleList();
 * console.log(result.stdout);
 * ```
 */
export async function lifecycleList() {
  return addon.lifecycleList();
}

/**
 * Check production readiness status
 *
 * @param {string} [environment] - Environment name (e.g., "staging", "production")
 * @returns {Promise<RunResult>} Readiness check result
 *
 * @example
 * ```javascript
 * const result = await lifecycleReadiness('production');
 * console.log(result.stdout);
 * ```
 */
export async function lifecycleReadiness(environment) {
  return addon.lifecycleReadiness(environment);
}

/**
 * Deploy project to target environment
 *
 * @param {string} [environment] - Environment name (defaults to "staging")
 * @returns {Promise<RunResult>} Deployment result
 *
 * @example
 * ```javascript
 * const result = await lifecycleDeploy('production');
 * console.log(result.stdout);
 * ```
 */
export async function lifecycleDeploy(environment) {
  return addon.lifecycleDeploy(environment);
}

/**
 * Validate project configuration and readiness
 *
 * @param {string} [environment] - Environment name (optional)
 * @returns {Promise<RunResult>} Validation result
 *
 * @example
 * ```javascript
 * const result = await lifecycleValidate('staging');
 * console.log(result.stdout);
 * ```
 */
export async function lifecycleValidate(environment) {
  return addon.lifecycleValidate(environment);
}

// ═══════════════════════════════════════════════════════════════════════════════
// AI-Powered Generation Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Generate code from a natural language description using AI
 *
 * @param {string} description - Natural language project description
 * @param {string} [projectName] - Optional project name
 * @param {string} [language] - Optional programming language
 * @returns {Promise<RunResult>} Generation result
 *
 * @example
 * ```javascript
 * const result = await aiGenerate(
 *   'REST API with user authentication',
 *   'my-api',
 *   'rust'
 * );
 * console.log(result.stdout);
 * ```
 */
export async function aiGenerate(description, projectName, language) {
  return addon.aiGenerate(description, projectName, language);
}

/**
 * Generate RDF ontology from project description
 *
 * @param {string} description - Project or domain description
 * @param {string} [outputPath] - Path to write ontology file
 * @returns {Promise<RunResult>} Ontology generation result
 *
 * @example
 * ```javascript
 * const result = await aiOntology(
 *   'E-commerce: products, orders, reviews',
 *   './domain.ttl'
 * );
 * console.log(result.stdout);
 * ```
 */
export async function aiOntology(description, outputPath) {
  return addon.aiOntology(description, outputPath);
}

/**
 * Generate SPARQL queries from description
 *
 * @param {string} description - Query description
 * @param {string} [graphPath] - Optional RDF graph path for context
 * @returns {Promise<RunResult>} SPARQL generation result
 *
 * @example
 * ```javascript
 * const result = await aiSparql(
 *   'Find all users who made purchases in 2024',
 *   './domain.ttl'
 * );
 * console.log(result.stdout);
 * ```
 */
export async function aiSparql(description, graphPath) {
  return addon.aiSparql(description, graphPath);
}

/**
 * Chat with AI about code generation
 *
 * @param {string} message - User message or question
 * @param {string} [context] - Optional context (code, ontology, etc.)
 * @returns {Promise<RunResult>} AI response in stdout
 *
 * @example
 * ```javascript
 * const result = await aiChat(
 *   'How should I structure my REST API?',
 *   'Domain: e-commerce'
 * );
 * console.log(result.stdout);
 * ```
 */
export async function aiChat(message, context) {
  return addon.aiChat(message, context);
}

/**
 * Analyze code and suggest improvements
 *
 * @param {string} filePath - Path to code file
 * @returns {Promise<RunResult>} Analysis result
 *
 * @example
 * ```javascript
 * const result = await aiAnalyze('./src/main.rs');
 * console.log(result.stdout);
 * ```
 */
export async function aiAnalyze(filePath) {
  return addon.aiAnalyze(filePath);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Utility Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Run system diagnostics and health checks
 *
 * @returns {Promise<RunResult>} Diagnostic results
 *
 * @example
 * ```javascript
 * const result = await doctor();
 * console.log(result.stdout);
 * ```
 */
export async function doctor() {
  return addon.doctor();
}

/**
 * Get help text (optionally for specific command)
 *
 * @param {string} [command] - Optional command name (e.g., "market", "template")
 * @returns {Promise<RunResult>} Help text in stdout
 *
 * @example
 * ```javascript
 * const result = await help('template');
 * console.log(result.stdout);
 * ```
 */
export async function help(command) {
  return addon.help(command);
}

/**
 * Print environment information
 *
 * @returns {Promise<RunResult>} Environment details
 *
 * @example
 * ```javascript
 * const result = await env();
 * console.log(result.stdout);
 * ```
 */
export async function env() {
  return addon.env();
}

// ═══════════════════════════════════════════════════════════════════════════════
// Legacy Compatibility Aliases
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * @deprecated Use marketSearch() instead
 * @param {string} query - Search query
 * @returns {Promise<RunResult>}
 */
export async function search(query) {
  return marketSearch(query);
}

/**
 * @deprecated Use templateList() instead
 * @returns {Promise<RunResult>}
 */
export async function list() {
  return templateList();
}

/**
 * @deprecated Use marketCategories() instead
 * @returns {Promise<RunResult>}
 */
export async function categories() {
  return marketCategories();
}

// ═══════════════════════════════════════════════════════════════════════════════
// Export for CommonJS compatibility (via dual export)
// ═══════════════════════════════════════════════════════════════════════════════

export default {
  version,
  run,
  marketSearch,
  marketList,
  marketCategories,
  marketAdd,
  marketRemove,
  templateList,
  templateShow,
  templateLint,
  templateGenerate,
  templateNew,
  projectNew,
  projectGenerate,
  projectWatch,
  projectInit,
  projectPlan,
  projectApply,
  lifecycleList,
  lifecycleReadiness,
  lifecycleDeploy,
  lifecycleValidate,
  aiGenerate,
  aiOntology,
  aiSparql,
  aiChat,
  aiAnalyze,
  doctor,
  help,
  env,
  // Legacy
  search,
  list,
  categories,
};
