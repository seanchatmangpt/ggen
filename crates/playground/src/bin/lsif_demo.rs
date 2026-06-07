use tower_lsp_max::lsp_types_max::{Position, Range};
use tower_lsp_max::lsif::lsif_builder::LsifBuilder;
use tower_lsp_max::lsif::lsif_types::{ToolInfo};

fn main() {
    let mut buffer = Vec::new();
    let mut builder = LsifBuilder::new(&mut buffer);

    builder
        .emit_metadata(
            "0.6.0",
            "file:///Users/sac/tower-lsp-max",
            ToolInfo {
                name: "lsif-demo".to_string(),
                version: Some("0.1.0".to_string()),
                args: None,
            },
        )
        .unwrap();

    let doc_id = builder
        .emit_document("file:///Users/sac/tower-lsp-max/src/lib.rs", "rust")
        .unwrap();

    let _range_id = builder
        .emit_range(
            Position { line: 0, character: 0 },
            Position { line: 0, character: 10 },
            None,
        )
        .unwrap();

    println!("{}", String::from_utf8(buffer).unwrap());
}
