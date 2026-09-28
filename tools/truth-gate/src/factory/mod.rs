//! Composable truth-gate policy factory.
pub mod boundary;
pub mod provenance;
pub mod authority;
pub mod receipt;
pub mod replay;
pub mod exclusion;
pub mod construct;
pub mod actuation;
pub mod evidence;
pub mod identity;
pub mod falsifier;
pub mod standing;
pub mod planner;
pub mod cost;
pub mod consequence;
pub mod recovery;
pub mod idempotency;
pub mod version;
pub mod schema;
pub mod serialization;
pub mod migration;
pub mod retry;
pub mod consumer;
pub mod compatibility;
pub mod ordering;
pub mod scope;
pub mod epoch;

#[derive(Debug,Clone,PartialEq,Eq)]pub struct PolicyDescriptor{pub name:&'static str,pub domain:&'static str}
pub fn catalog()->Vec<PolicyDescriptor>{vec![PolicyDescriptor{name:"boundary",domain:"admission"},PolicyDescriptor{name:"provenance",domain:"admission"},PolicyDescriptor{name:"authority",domain:"admission"},PolicyDescriptor{name:"receipt",domain:"admission"},PolicyDescriptor{name:"replay",domain:"admission"},PolicyDescriptor{name:"exclusion",domain:"admission"},PolicyDescriptor{name:"construct",domain:"admission"},PolicyDescriptor{name:"actuation",domain:"admission"},PolicyDescriptor{name:"evidence",domain:"admission"},PolicyDescriptor{name:"identity",domain:"admission"},PolicyDescriptor{name:"falsifier",domain:"admission"},PolicyDescriptor{name:"standing",domain:"admission"},PolicyDescriptor{name:"planner",domain:"admission"},PolicyDescriptor{name:"cost",domain:"admission"},PolicyDescriptor{name:"consequence",domain:"admission"},PolicyDescriptor{name:"recovery",domain:"admission"},PolicyDescriptor{name:"idempotency",domain:"admission"},PolicyDescriptor{name:"version",domain:"admission"},PolicyDescriptor{name:"schema",domain:"admission"},PolicyDescriptor{name:"serialization",domain:"admission"},PolicyDescriptor{name:"migration",domain:"admission"},PolicyDescriptor{name:"retry",domain:"admission"},PolicyDescriptor{name:"consumer",domain:"admission"},PolicyDescriptor{name:"compatibility",domain:"admission"},PolicyDescriptor{name:"ordering",domain:"admission"},PolicyDescriptor{name:"scope",domain:"admission"},PolicyDescriptor{name:"epoch",domain:"admission"} ]}
pub fn contains(name:&str)->bool{catalog().iter().any(|p|p.name==name)}
#[cfg(test)]mod tests{use super::*;#[test]fn catalog_is_complete(){assert_eq!(catalog().len(),27);}#[test]fn lookup_is_exact(){assert!(contains("boundary"));assert!(!contains("bound"));}}
