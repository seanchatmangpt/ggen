//! P2P Marketplace Registry Implementation
//!
//! Provides a distributed, peer-to-peer marketplace for package discovery and distribution
//! using libp2p with Kademlia DHT, Gossipsub, and content routing.

pub mod registry;
pub mod behaviour;
pub mod config;
pub mod discovery;
pub mod content;
pub mod protocol;
pub mod types;

pub use registry::{P2PRegistry, P2PRegistryBuilder};
pub use behaviour::MarketplaceBehaviour;
pub use config::{P2PConfig, NetworkConfig, SecurityConfig};
pub use discovery::{PeerDiscovery, BootstrapConfig};
pub use content::{ContentRouter, ContentProvider};
pub use protocol::{PackageProtocol, RequestResponse};
pub use types::{Package, PackageMetadata, Query, SearchResult};
