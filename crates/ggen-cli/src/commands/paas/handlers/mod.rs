//! PaaS command handlers
//!
//! Each module implements a single verb handler using Result<T, PaasError>

pub mod init;
pub mod update;
pub mod validate;
pub mod sync;
pub mod deploy;
pub mod status;
pub mod logs;
pub mod describe;
pub mod explain;

pub use init::init_submodule;
pub use update::update_submodule;
pub use validate::validate_specs;
pub use sync::sync_specs;
pub use deploy::deploy_artifacts;
pub use status::show_status;
pub use logs::stream_logs;
pub use describe::describe_resource;
pub use explain::explain_artifact;
