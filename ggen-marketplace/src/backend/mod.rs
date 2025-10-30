pub mod centralized;
pub mod local;

#[cfg(feature = "p2p")]
pub mod p2p;

pub use centralized::CentralizedRegistry;
pub use local::LocalRegistry;

#[cfg(feature = "p2p")]
pub use p2p::P2PRegistry;
