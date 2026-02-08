//! Code generation module for YAWL workflows.

pub mod yawl_xml;

pub use yawl_xml::{YawlXmlGenerator, canonicalize, validate};
