//! Deterministic manufacturing engine for Vision 2030
//!
//! This module implements the manufacturing operators and proof gates required for
//! verifiable artifact production.

pub mod gates;
pub mod operator;

pub use gates::{GateResult, ProofGate};
pub use operator::{ManufacturingOperator, OperatorContext};
