/**
 * Hyper-Thesis Framework (HTF) - Complete Index
 *
 * Exports all HTF components and utilities for unified thesis validation
 * and planning across 7 thesis families, 27 shards, and 9 subsystems.
 */

// Core HTF Components
export { HTFVisualizer } from './HTFVisualizer'
export { HTFDashboard } from './HTFDashboard'
export { LambdaScheduler } from './LambdaScheduler'
export { PiProfile } from './PiProfile'
export { GammaChecker } from './GammaChecker'

// Re-export defaults for convenience
export { default as HTFVisualizerDefault } from './HTFVisualizer'
export { default as HTFDashboardDefault } from './HTFDashboard'
export { default as LambdaSchedulerDefault } from './LambdaScheduler'
export { default as PiProfileDefault } from './PiProfile'
export { default as GammaCheckerDefault } from './GammaChecker'

/**
 * HTF Architecture Overview
 *
 * The Hyper-Thesis Framework unifies 7 thesis modes through 4 mechanisms:
 *
 * 1. DELTA (Δ) SHARDS (27 total)
 *    - Discrete thesis components organized by family
 *    - 7 families × 3-5 shards = 27 total components
 *    - Each shard maps to specific thesis content
 *
 * 2. LAMBDA (Λ) ORDERING (20 constraints)
 *    - Total ordering of chapters ensuring logical flow
 *    - Primordial: problem ≺ gap ≺ claim ≺ introduction
 *    - Evidence: artifact ≺ ground ≺ papers ≺ results ≺ proofs
 *    - Synthesis: reply ≺ theory ≺ analysis ≺ conclusion ≺ insight
 *    - Enforced by: LambdaScheduler component
 *
 * 3. PI (Π) MERGE (63+ mappings)
 *    - Integration of 9 subsystems across 27 shards
 *    - Each subsystem appears in 3-8 shards across multiple families
 *    - Creates unified narrative across all thesis modes
 *    - Enforced by: PiProfile component
 *
 * 4. GAMMA (Γ) GLUING (Coherence validation)
 *    - Sheaf-like integration ensuring no contradictions
 *    - Global Invariants (Q): 6 properties that must always hold
 *    - Family Coherence: Each family internally consistent
 *    - Drift Detection: Monitors deviation from core claims
 *    - Enforced by: GammaChecker component
 *
 * CORE SUBSYSTEMS (9)
 * 1. Decentralized Identifiers (DIDs)
 * 2. Zero-Knowledge Proofs & Verifiable Credentials
 * 3. Autonomous Agents
 * 4. Quantum-Safe Cryptography
 * 5. Agent Swarm Coordination
 * 6. Consensus & Governance Proposals
 * 7. Reputation Systems
 * 8. Biometric Authentication
 * 9. Blockchain Integration
 *
 * THESIS FAMILIES (7)
 * 1. IMRaD: Introduction, Method, Results, Discussion
 * 2. Papers: Research papers with synthesis paper
 * 3. Argument: Claims, grounds, proofs, objections, replies
 * 4. Contribution: Gap, design, evaluation, impact
 * 5. Monograph: Context, canon, method, analysis, conclusion
 * 6. DSR: Design Science Research (problem, artifact, evaluation, theory)
 * 7. Narrative: Field, voice, pattern, insight
 *
 * GLOBAL INVARIANTS (Q)
 * Q1: All 9 subsystems covered in at least 3 families
 * Q2: Main claim grounded in evidence and theory
 * Q3: No contradictions between thesis sections
 * Q4: Coherent narrative arc from problem to insight
 * Q5: Sufficient technical depth (>4000 words in Papers/Contribution)
 * Q6: Strong defense against objections with replies
 *
 * USAGE
 * 1. Use HTFDashboard as main entry point
 * 2. Use LambdaScheduler for chapter planning
 * 3. Use PiProfile to verify subsystem coverage
 * 4. Use GammaChecker to validate coherence
 * 5. Use HTFVisualizer for detailed shard inspection
 */

export const HTF_VERSION = '1.0.0'
export const HTF_RELEASE_DATE = '2024-11-18'

export interface HTFConfig {
  totalShards: 14
  totalFamilies: 7
  totalSubsystems: 9
  lambdaConstraints: 20
  globalInvariants: 6
  driftRules: 5
}

export const HTF_CONFIG: HTFConfig = {
  totalShards: 27,
  totalFamilies: 7,
  totalSubsystems: 9,
  lambdaConstraints: 20,
  globalInvariants: 6,
  driftRules: 5,
}

export const HTF_FAMILIES = [
  'IMRaD',
  'Papers',
  'Argument',
  'Contribution',
  'Monograph',
  'DSR',
  'Narrative',
] as const

export const HTF_SUBSYSTEMS = [
  'DIDs',
  'Zero-Knowledge Proofs & VCs',
  'Autonomous Agents',
  'Quantum-Safe Cryptography',
  'Agent Swarm Coordination',
  'Consensus & Governance',
  'Reputation Systems',
  'Biometric Authentication',
  'Blockchain Integration',
] as const
