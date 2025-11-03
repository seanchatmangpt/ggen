pub mod centralized;
pub mod local;

#[cfg(feature = "p2p")]
pub mod p2p;

#[cfg(feature = "graphql-server")]
pub mod content_distribution;

pub use centralized::CentralizedRegistry;
pub use local::LocalRegistry;

#[cfg(feature = "p2p")]
pub use p2p::P2PRegistry;

#[cfg(feature = "graphql-server")]
pub use content_distribution::{ContentDistributionConfig, ContentDistributionServer};
