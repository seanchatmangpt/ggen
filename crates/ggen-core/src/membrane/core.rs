//! Membrane core bindings for the Genesis embedded runtime.
//!
//! Provides the outer membrane, adapter layer, and execution boundaries for Genesis.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::time::SystemTime;
use uuid::Uuid;

use crate::utils::error::Result;

/// Logical clock and vector clock tracking for execution boundaries
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
pub struct VectorClock {
    /// Mapping of node/component IDs to logical timestamps
    pub clocks: BTreeMap<String, u64>,
}

impl VectorClock {
    /// Create a new VectorClock
    pub fn new() -> Self {
        Self {
            clocks: BTreeMap::new(),
        }
    }

    /// Increment logical time for a specific node
    pub fn tick(&mut self, node_id: &str) {
        let entry = self.clocks.entry(node_id.to_string()).or_insert(0);
        *entry += 1;
    }

    /// Merge another vector clock
    pub fn merge(&mut self, other: &VectorClock) {
        for (node_id, &time) in &other.clocks {
            let entry = self.clocks.entry(node_id.clone()).or_insert(0);
            *entry = std::cmp::max(*entry, time);
        }
    }
}

/// An execution boundary crossing event captured by the membrane
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BoundaryCrossing {
    /// Unique event identifier
    pub id: String,
    /// Type of crossing (e.g. Call, Return, Attestation, StateChange)
    pub crossing_type: String,
    /// Node or component initiating the crossing
    pub caller_id: String,
    /// Target component being invoked
    pub callee_id: String,
    /// Function/trait being executed
    pub interface_fn: String,
    /// Real timestamp of the crossing
    pub timestamp: DateTime<Utc>,
    /// Vector clock state at the time of crossing
    pub vector_clock: VectorClock,
    /// BLAKE3 hash of input payload
    pub input_hash: String,
    /// BLAKE3 hash of output payload (if returned)
    pub output_hash: Option<String>,
    /// Status code of execution (0 for success)
    pub status_code: i32,
    /// Execution duration in microseconds
    pub duration_us: u64,
}

/// Definition of an interchangeable part loaded into the Genesis runtime
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterchangeablePart {
    /// Unique identifier of the component
    pub id: String,
    /// Part type (e.g., "wasm32", "atomvm_beam", "native")
    pub part_type: String,
    /// Semantic version of the part
    pub version: String,
    /// Interfaces/traits implemented by the part
    pub interfaces: Vec<String>,
    /// BLAKE3 hash of the part's binary payload
    pub payload_hash: String,
    /// Size of payload in bytes
    pub payload_size: usize,
}

/// Genesis embedded core instance wrapped by the ggen membrane
pub struct GenesisCore {
    /// Registered interchangeable parts
    pub parts: HashMap<String, InterchangeablePart>,
    /// Active state of loaded parts (simulated or real memory mappings)
    pub memory_states: HashMap<String, Vec<u8>>,
    /// Vector clock tracking for the overall runtime
    pub runtime_clock: VectorClock,
}

impl GenesisCore {
    /// Create a new Genesis embedded core
    pub fn new() -> Self {
        Self {
            parts: HashMap::new(),
            memory_states: HashMap::new(),
            runtime_clock: VectorClock::new(),
        }
    }

    /// Register an interchangeable part with the core
    pub fn register_part(&mut self, part: InterchangeablePart) -> Result<()> {
        let part_id = part.id.clone();
        self.runtime_clock.tick("genesis-core");
        self.parts.insert(part_id, part);
        Ok(())
    }

    /// Set initial memory state/data for a part
    pub fn set_state(&mut self, part_id: &str, state: Vec<u8>) -> Result<()> {
        self.memory_states.insert(part_id.to_string(), state);
        Ok(())
    }
}

/// The outer membrane layer managing adapters, events, and boundary crossings
pub struct GgenMembrane {
    /// Reference to the embedded core
    pub core: GenesisCore,
    /// Log of all boundary crossings
    pub event_log: Vec<BoundaryCrossing>,
    /// Registered adapters mapping outer inputs to inner interfaces
    pub adapters: HashMap<String, String>,
}

impl GgenMembrane {
    /// Create a new membrane wrapping the Genesis core
    pub fn new(core: GenesisCore) -> Self {
        Self {
            core,
            event_log: Vec::new(),
            adapters: HashMap::new(),
        }
    }

    /// Register an adapter binding between an outer port and inner component interface
    pub fn bind_adapter(&mut self, outer_port: &str, inner_interface: &str) {
        self.adapters
            .insert(outer_port.to_string(), inner_interface.to_string());
    }

    /// Execute an operations crossing the membrane boundary into the Genesis core
    pub fn invoke(
        &mut self, caller: &str, callee_part_id: &str, interface_fn: &str, input_data: &[u8],
    ) -> Result<(Vec<u8>, BoundaryCrossing)> {
        let start_time = Instant::new();
        let utc_now = Utc::now();

        // Calculate cryptographic evidence for the input payload using BLAKE3
        let input_hash = blake3::hash(input_data).to_hex().to_string();

        // Increment clocks
        self.core.runtime_clock.tick(callee_part_id);
        self.core.runtime_clock.tick(caller);
        let current_clock = self.core.runtime_clock.clone();

        // Check if the part exists
        let (output_data, status_code) = if let Some(part) = self.core.parts.get(callee_part_id) {
            // Emulate execution logic on the interchangeable part
            // For a production system, this executes WASM / AtomVM code.
            // Here, we provide a deterministic projection layer output based on input.
            let mut output = Vec::new();
            output.extend_from_slice(b"Executed: ");
            output.extend_from_slice(part.id.as_bytes());
            output.extend_from_slice(b"::");
            output.extend_from_slice(interface_fn.as_bytes());
            output.extend_from_slice(b" on data: ");
            output.extend_from_slice(input_data);

            (output, 0)
        } else {
            (b"Part not found".to_vec(), 404)
        };

        // Calculate cryptographic evidence for the output payload using BLAKE3
        let output_hash = blake3::hash(&output_data).to_hex().to_string();
        let duration = start_time.elapsed().as_micros() as u64;

        let crossing = BoundaryCrossing {
            id: Uuid::new_v4().to_string(),
            crossing_type: "CallAndReturn".to_string(),
            caller_id: caller.to_string(),
            callee_id: callee_part_id.to_string(),
            interface_fn: interface_fn.to_string(),
            timestamp: utc_now,
            vector_clock: current_clock,
            input_hash,
            output_hash: Some(output_hash),
            status_code,
            duration_us: duration,
        };

        self.event_log
            .clone_from(&[self.event_log.clone(), vec![crossing.clone()]].concat());

        Ok((output_data, crossing))
    }
}

// Support timing helpers without std::time::Instant limits
struct Instant(SystemTime);
impl Instant {
    fn new() -> Self {
        Self(SystemTime::now())
    }
    fn elapsed(&self) -> std::time::Duration {
        self.0
            .elapsed()
            .unwrap_or(std::time::Duration::from_secs(0))
    }
}
