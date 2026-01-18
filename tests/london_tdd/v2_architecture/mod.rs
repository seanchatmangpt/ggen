//! London TDD tests for ggen v2.0 async/sync wrapper architecture
//!
//! This module contains comprehensive tests for the v2.0 architecture:
//! - cmds (clap integration) → domain (async logic)
//! - runtime::execute bridge for sync wrappers
//! - OTEL integration
//!
//! Test distribution (London TDD):
//! - 20% Unit tests (runtime bridge, utilities)
//! - 60% Component tests (domain logic with mocks)
//! - 20% Integration tests (CLI → domain → output)
//!
//! Performance target: Full suite <1s

pub mod unit;
pub mod component;
pub mod integration;
