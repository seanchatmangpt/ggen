mod common;

use common::{create_temp_dir, write_template_file, TestLspClient};
use serde_json::json;

#[test]
fn test_harness_temp_dir_and_write() {
    let tmp = create_temp_dir().expect("create temp dir");
    let content = "hello template";
    let path =
        write_template_file(tmp.path(), "sub/dir/test.tmpl", content).expect("write template");

    assert!(path.exists());
    let read_back = std::fs::read_to_string(path).expect("read file");
    assert_eq!(read_back, content);
}

#[test]
fn test_harness_lsp_communication() {
    let mut client = TestLspClient::spawn().expect("spawn lsp server");

    // Initialize standard LSP handshake
    let init_resp = client
        .request(
            "initialize",
            json!({
                "processId": null,
                "rootUri": null,
                "capabilities": {}
            }),
        )
        .expect("initialize LSP");

    assert!(init_resp.get("result").is_some());

    // Send standard initialized notification
    client
        .notify("initialized", json!({}))
        .expect("send initialized notification");

    // Shutdown & exit
    let shutdown_resp = client
        .request("shutdown", json!(null))
        .expect("shutdown LSP");
    assert!(shutdown_resp.get("result").is_some());
}
