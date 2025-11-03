/**
 * Production-grade TypeScript type definitions for @ggen/node
 * Node.js N-API bindings for the ggen CLI toolkit
 */

/** Result of a CLI command execution */
export interface RunResult {
  /** Exit code (0 = success, non-zero = error) */
  code: number;
  /** Standard output captured from the command */
  stdout: string;
  /** Standard error captured from the command */
  stderr: string;
}

/** Get the ggen version */
export function version(): string;

/**
 * Low-level entrypoint to invoke the CLI.
 * Prefer higher-level helpers unless you need raw access.
 */
export function run(args: string[]): Promise<RunResult>;

// ═══════════════════════════════════════════════════════════════════════════════
// Marketplace Commands
// ═══════════════════════════════════════════════════════════════════════════════

/** Search marketplace packages by query */
export function marketSearch(query: string): Promise<RunResult>;

/** Add a marketplace package to the current project */
export function marketAdd(packageId: string): Promise<RunResult>;

/** List all installed marketplace packages */
export function marketList(): Promise<RunResult>;

/** List available marketplace categories */
export function marketCategories(): Promise<RunResult>;

/** Remove a marketplace package from the current project */
export function marketRemove(packageId: string): Promise<RunResult>;

// ═══════════════════════════════════════════════════════════════════════════════
// Lifecycle Commands
// ═══════════════════════════════════════════════════════════════════════════════

/** Initialize a new ggen project */
export function lifecycleInit(): Promise<RunResult>;

/** Run tests for the current project */
export function lifecycleTest(): Promise<RunResult>;

/** Build the current project */
export function lifecycleBuild(): Promise<RunResult>;

/** Deploy the project to a specified environment */
export function lifecycleDeploy(env?: string): Promise<RunResult>;

/** Validate deployment readiness for a specified environment */
export function lifecycleValidate(env?: string): Promise<RunResult>;

/** Check production readiness status */
export function lifecycleReadiness(): Promise<RunResult>;

/** Update readiness status for a specific requirement */
export function lifecycleReadinessUpdate(requirementId: string, status: string): Promise<RunResult>;

/** List available lifecycle phases */
export function lifecycleList(): Promise<RunResult>;

// ═══════════════════════════════════════════════════════════════════════════════
// Template Generation Commands
// ═══════════════════════════════════════════════════════════════════════════════

/** Generate code from a template */
export function templateGenerate(
  templatePath: string,
  vars?: Record<string, string> | null,
  manifestPath?: string | null
): Promise<RunResult>;

/** List available templates */
export function templateList(): Promise<RunResult>;

// ═══════════════════════════════════════════════════════════════════════════════
// AI Generation Commands
// ═══════════════════════════════════════════════════════════════════════════════

/** Generate a complete project using AI from a description */
export function aiProject(
  description: string,
  name?: string | null,
  language?: string | null
): Promise<RunResult>;

/** Generate a template file using AI from a description */
export function aiGenerate(description: string, outputPath: string): Promise<RunResult>;

/** Generate an RDF ontology using AI from a description */
export function aiGraph(description: string, outputPath: string): Promise<RunResult>;

/** Generate a SPARQL query using AI from a description */
export function aiSparql(description: string, graphPath?: string | null): Promise<RunResult>;

// ═══════════════════════════════════════════════════════════════════════════════
// Utility Commands
// ═══════════════════════════════════════════════════════════════════════════════

/** Run environment diagnostics */
export function doctor(): Promise<RunResult>;

/** Get help text for a specific command */
export function help(command?: string | null): Promise<RunResult>;

// ═══════════════════════════════════════════════════════════════════════════════
// Legacy Compatibility (maintained for backward compatibility)
// ═══════════════════════════════════════════════════════════════════════════════

/** @deprecated Use templateGenerate instead */
export interface GenOptions {
  vars?: Record<string, string>;
  manifestPath?: string;
}

/** @deprecated Use templateGenerate instead */
export function gen(templatePath: string, options?: GenOptions): Promise<RunResult>;

/** @deprecated Use marketSearch instead */
export function search(query: string): Promise<RunResult>;

/** @deprecated Use marketAdd instead */
export function add(packageId: string): Promise<RunResult>;

/** @deprecated Use templateList instead */
export function list(): Promise<RunResult>;

/** @deprecated Use marketCategories instead */
export function categories(): Promise<RunResult>;

/** @deprecated Use aiProject instead */
export interface AiProjectFlags {
  name?: string;
  rust?: boolean;
  typescript?: boolean;
  python?: boolean;
  go?: boolean;
}

/** @deprecated Use aiProject instead */
export function aiProject(description: string, flags?: AiProjectFlags): Promise<RunResult>;

/** @deprecated Use aiGenerate instead */
export function aiGenerate(description: string, outputPath: string): Promise<RunResult>;

/** @deprecated Use aiGraph instead */
export function aiGraph(description: string, outputPath: string): Promise<RunResult>;

/** @deprecated Use aiSparql instead */
export function aiSparql(description: string, graphPath?: string): Promise<RunResult>;

/** @deprecated Use a custom GitHub integration library */
export function githubPagesStatus(): Promise<RunResult>;
