use thiserror::Error;

#[derive(Debug, Error)]
pub enum LspError {
    #[error("RDF parse error: {0}")]
    RdfParseError(String),

    #[error("RDF query error: {0}")]
    RdfQueryError(String),

    #[error("Tera parse error: {0}")]
    TeraParseError(String),

    #[error("Tera render error: {0}")]
    TeraRenderError(String),

    #[error("TOML parse error: {0}")]
    TomlParseError(String),

    #[error("TOML validation error: {0}")]
    TomlValidationError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("LSP protocol error: {0}")]
    LspProtocolError(String),

    #[error("Unknown error: {0}")]
    Unknown(String),
}

impl From<LspError> for tower_lsp_max::jsonrpc::Error {
    fn from(err: LspError) -> Self {
        tower_lsp_max::jsonrpc::Error {
            code: tower_lsp_max::jsonrpc::ErrorCode::InternalError,
            message: err.to_string().into(),
            data: None,
        }
    }
}

pub type Result<T> = std::result::Result<T, LspError>;
