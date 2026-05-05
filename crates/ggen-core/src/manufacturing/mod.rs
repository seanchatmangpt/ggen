//! Deterministic manufacturing engine for Vision 2030
//!
//! This module implements the manufacturing operators and proof gates required for 
//! verifiable artifact production.

pub mod operator;
pub mod gates;

pub use operator::{ManufacturingOperator, OperatorContext};
pub use gates::{ProofGate, GateResult};