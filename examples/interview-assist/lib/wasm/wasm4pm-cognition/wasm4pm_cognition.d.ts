/* tslint:disable */
/* eslint-disable */
/**
 * Verify a persisted session state without admitting a new turn.
 */
export function cognition_session_verify(input_json: string): any;
/**
 * Replay-verify state and return the canonical Python artifact selected by cognition.
 */
export function cognition_session_code(input_json: string): any;
/**
 * Execute one session turn through the sovereign WASM boundary.
 */
export function cognition_session_turn(input_json: string): any;
/**
 * Show cognition capabilities report.
 *   Validated Doctest Example:
 * ```rust
 * // Validation successful
 * ```
 */
export function cognition_show(): any;
/**
 * Run cognition contract with breed execution. Strict input validation:
 * 10 MiB cap, schema with `deny_unknown_fields`, breed length bounds.
 *   Validated Doctest Example:
 * ```rust
 * // Validation successful
 * ```
 */
export function cognition_run(input_json: string): any;
/**
 * Verify a result against adversarial gates. Length-bounded.
 *   Validated Doctest Example:
 * ```rust
 * // Validation successful
 * ```
 */
export function cognition_verify(result_json: string): any;
/**
 * Replay a receipt by run_id (length-bounded).
 *   Validated Doctest Example:
 * ```rust
 * // Validation successful
 * ```
 */
export function cognition_replay(run_id: string): any;
/**
 * Build an architecture system given intent. Parses manifest and computes Pareto frontier.
 *   Validated Doctest Example:
 * ```rust
 * // Validation successful
 * ```
 */
export function system_build(intent_json: string): any;
/**
 * Verify a target system against artifacts.
 *   Validated Doctest Example:
 * ```rust
 * // Validation successful
 * ```
 */
export function system_verify(target: string, artifacts_json: string): any;
