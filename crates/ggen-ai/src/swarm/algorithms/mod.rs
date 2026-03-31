//! Swarm Intelligence Algorithms
//!
//! This module provides swarm intelligence algorithms for optimizing various aspects
//! of code generation, including:
//! - Ant Colony Optimization (ACO) for SPARQL query path finding
//! - Particle Swarm Optimization (PSO) for template parameter tuning
//! - Collaborative evolution for template optimization

pub mod aco;
pub mod evolution;
pub mod pso;

pub use aco::*;
pub use evolution::*;
pub use pso::*;
