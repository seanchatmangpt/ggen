/**
 * Production-grade Node.js N-API bindings for ggen CLI
 *
 * This module provides high-performance, type-safe bindings to the ggen CLI
 * for use in Node.js applications.
 *
 * @packageDocumentation
 */

/* eslint-disable @typescript-eslint/no-var-requires */
const addon = require('./index.node');

export interface RunResult {
  code: number;
  stdout: string;
  stderr: string;
}

// ═══════════════════════════════════════════════════════════════════════════════
// Core Functions
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Get the ggen version (matches crate version)
 *
 * @example
 * ```typescript
 * import { version } from '@ggen/node';
 * console.log('ggen version:', version());
 * ```
 */
export function version(): string {
  return addon.version();
}

/**
 * Low-level entrypoint to invoke the CLI.
 * Prefer higher-level wrapper functions for better ergonomics.
 *
 * @param args - Command-line arguments (without the 'ggen' binary name)
 * @returns RunResult containing exit code and captured stdout/stderr
 *
 * @example
 * ```typescript
 * const result = await run(['--version']);
 * console.log(result.stdout);
 * ```
 */
export async function run(args: string[]): Promise<RunResult> {
  return addon.run(args);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Marketplace Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Search marketplace packages by query
 *
 * @param query - Search terms (e.g., "rust web service")
 *
 * @example
 * ```typescript
 * const result = await marketSearch('rust web');
 * console.log(result.stdout);
 * ```
 */
export async function marketSearch(query: string): Promise<RunResult> {
  return addon.market_search(query);
}

/**
 * Add a marketplace package to the current project
 *
 * @param packageId - Package identifier (e.g., "io.ggen.rust.axum-service")
 *
 * @example
 * ```typescript
 * await marketAdd('io.ggen.rust.axum-service');
 * ```
 */
export async function marketAdd(packageId: string): Promise<RunResult> {
  return addon.market_add(packageId);
}

/**
 * List all installed marketplace packages
 *
 * @example
 * ```typescript
 * const result = await marketList();
 * console.log(result.stdout);
 * ```
 */
export async function marketList(): Promise<RunResult> {
  return addon.market_list();
}

/**
 * List available marketplace categories
 *
 * @example
 * ```typescript
 * const result = await marketCategories();
 * console.log(result.stdout);
 * ```
 */
export async function marketCategories(): Promise<RunResult> {
  return addon.market_categories();
}

/**
 * Remove a marketplace package from the current project
 *
 * @param packageId - Package identifier to remove
 *
 * @example
 * ```typescript
 * await marketRemove('io.ggen.rust.axum-service');
 * ```
 */
export async function marketRemove(packageId: string): Promise<RunResult> {
  return addon.market_remove(packageId);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Lifecycle Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Initialize a new ggen project
 *
 * @example
 * ```typescript
 * await lifecycleInit();
 * ```
 */
export async function lifecycleInit(): Promise<RunResult> {
  return addon.lifecycle_init();
}

/**
 * Run tests for the current project
 *
 * @example
 * ```typescript
 * const result = await lifecycleTest();
 * if (result.code !== 0) {
 *   console.error('Tests failed:', result.stderr);
 * }
 * ```
 */
export async function lifecycleTest(): Promise<RunResult> {
  return addon.lifecycle_test();
}

/**
 * Build the current project
 *
 * @example
 * ```typescript
 * await lifecycleBuild();
 * ```
 */
export async function lifecycleBuild(): Promise<RunResult> {
  return addon.lifecycle_build();
}

/**
 * Deploy the project to a specified environment
 *
 * @param env - Target environment (e.g., "staging", "production")
 *
 * @example
 * ```typescript
 * await lifecycleDeploy('production');
 * ```
 */
export async function lifecycleDeploy(env?: string): Promise<RunResult> {
  return addon.lifecycle_deploy(env ?? null);
}

/**
 * Validate deployment readiness for a specified environment
 *
 * @param env - Target environment to validate (e.g., "production")
 *
 * @example
 * ```typescript
 * const result = await lifecycleValidate('production');
 * if (result.code === 0) {
 *   console.log('Ready for production deployment');
 * }
 * ```
 */
export async function lifecycleValidate(env?: string): Promise<RunResult> {
  return addon.lifecycle_validate(env ?? null);
}

/**
 * Check production readiness status
 *
 * @example
 * ```typescript
 * const result = await lifecycleReadiness();
 * console.log(result.stdout);
 * ```
 */
export async function lifecycleReadiness(): Promise<RunResult> {
  return addon.lifecycle_readiness();
}

/**
 * Update readiness status for a specific requirement
 *
 * @param requirementId - Requirement identifier
 * @param status - New status value (e.g., "complete", "in-progress")
 *
 * @example
 * ```typescript
 * await lifecycleReadinessUpdate('auth-basic', 'complete');
 * ```
 */
export async function lifecycleReadinessUpdate(
  requirementId: string,
  status: string
): Promise<RunResult> {
  return addon.lifecycle_readiness_update(requirementId, status);
}

/**
 * List available lifecycle phases
 *
 * @example
 * ```typescript
 * const result = await lifecycleList();
 * console.log(result.stdout);
 * ```
 */
export async function lifecycleList(): Promise<RunResult> {
  return addon.lifecycle_list();
}

// ═══════════════════════════════════════════════════════════════════════════════
// Template Generation Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Generate code from a template
 *
 * @param templatePath - Path to template file
 * @param vars - Optional variable map for template rendering
 * @param manifestPath - Optional path to ggen.toml manifest
 *
 * @example
 * ```typescript
 * await templateGenerate('service.tmpl', { name: 'api' });
 * ```
 */
export async function templateGenerate(
  templatePath: string,
  vars?: Record<string, string> | null,
  manifestPath?: string | null
): Promise<RunResult> {
  return addon.template_generate(templatePath, vars ?? null, manifestPath ?? null);
}

/**
 * List available templates
 *
 * @example
 * ```typescript
 * const result = await templateList();
 * console.log(result.stdout);
 * ```
 */
export async function templateList(): Promise<RunResult> {
  return addon.template_list();
}

// ═══════════════════════════════════════════════════════════════════════════════
// AI Generation Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Generate a complete project using AI from a description
 *
 * @param description - Natural language project description
 * @param name - Optional project name
 * @param language - Optional target language (rust, typescript, python, go)
 *
 * @example
 * ```typescript
 * await aiProject('REST API with authentication', 'my-api', 'rust');
 * ```
 */
export async function aiProject(
  description: string,
  name?: string | null,
  language?: string | null
): Promise<RunResult> {
  return addon.ai_project(description, name ?? null, language ?? null);
}

/**
 * Generate a template file using AI from a description
 *
 * @param description - What the template should do
 * @param outputPath - Where to save the generated template
 *
 * @example
 * ```typescript
 * await aiGenerate('Database repository for users', 'user-repo.tmpl');
 * ```
 */
export async function aiGenerate(description: string, outputPath: string): Promise<RunResult> {
  return addon.ai_generate(description, outputPath);
}

/**
 * Generate an RDF ontology using AI from a description
 *
 * @param description - What the ontology should represent
 * @param outputPath - Where to save the generated ontology (.ttl file)
 *
 * @example
 * ```typescript
 * await aiGraph('User management ontology', 'users.ttl');
 * ```
 */
export async function aiGraph(description: string, outputPath: string): Promise<RunResult> {
  return addon.ai_graph(description, outputPath);
}

/**
 * Generate a SPARQL query using AI from a description
 *
 * @param description - What the query should find
 * @param graphPath - Optional path to RDF graph file for context
 *
 * @example
 * ```typescript
 * await aiSparql('Find all active users', 'users.ttl');
 * ```
 */
export async function aiSparql(description: string, graphPath?: string | null): Promise<RunResult> {
  return addon.ai_sparql(description, graphPath ?? null);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Utility Commands
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * Run environment diagnostics
 *
 * @example
 * ```typescript
 * const result = await doctor();
 * console.log(result.stdout);
 * ```
 */
export async function doctor(): Promise<RunResult> {
  return addon.doctor();
}

/**
 * Get help text for a specific command
 *
 * @param command - Optional command name (empty for general help)
 *
 * @example
 * ```typescript
 * const result = await help('market');
 * console.log(result.stdout);
 * ```
 */
export async function help(command?: string | null): Promise<RunResult> {
  return addon.help(command ?? null);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Legacy Compatibility (maintained for backward compatibility)
// ═══════════════════════════════════════════════════════════════════════════════

/**
 * @deprecated Use templateGenerate instead
 */
export interface GenOptions {
  vars?: Record<string, string>;
  manifestPath?: string;
}

/**
 * @deprecated Use templateGenerate instead
 */
export async function gen(templatePath: string, options: GenOptions = {}): Promise<RunResult> {
  const vars = options.vars ? options.vars : null;
  const manifestPath = options.manifestPath ? options.manifestPath : null;
  return templateGenerate(templatePath, vars, manifestPath);
}

/**
 * @deprecated Use marketSearch instead
 */
export async function search(query: string): Promise<RunResult> {
  return marketSearch(query);
}

/**
 * @deprecated Use marketAdd instead
 */
export async function add(packageId: string): Promise<RunResult> {
  return marketAdd(packageId);
}

/**
 * @deprecated Use templateList instead
 */
export async function list(): Promise<RunResult> {
  return templateList();
}

/**
 * @deprecated Use marketCategories instead
 */
export async function categories(): Promise<RunResult> {
  return marketCategories();
}

/**
 * @deprecated Use aiProject instead
 */
export interface AiProjectFlags {
  name?: string;
  rust?: boolean;
  typescript?: boolean;
  python?: boolean;
  go?: boolean;
}

/**
 * @deprecated GitHub Pages functionality removed. Use a custom GitHub integration library.
 */
export async function githubPagesStatus(): Promise<RunResult> {
  throw new Error('githubPagesStatus is deprecated and has been removed');
}
