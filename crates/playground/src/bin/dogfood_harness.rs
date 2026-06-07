use std::sync::Arc;
use std::collections::HashMap;
use std::str::FromStr;
use serde_json::{json, Value};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tower_lsp_max::lsp_types::*;
use tower_lsp_max::{ComposedServer, LspService, Server};

// --- WORKSPACE INDEXER ---

struct SimpleWorkspaceIndex {
    definitions: HashMap<String, Vec<Location>>,
    references: HashMap<String, Vec<Location>>,
}

impl SimpleWorkspaceIndex {
    fn new() -> Self {
        Self {
            definitions: HashMap::new(),
            references: HashMap::new(),
        }
    }

    fn index_dir(&mut self, path: &str) {
        let mut dirs = vec![std::path::PathBuf::from(path)];
        while let Some(dir) = dirs.pop() {
            if let Ok(entries) = std::fs::read_dir(dir) {
                for entry in entries.flatten() {
                    let p = entry.path();
                    if p.is_dir() {
                        let name = p.file_name().unwrap_or_default().to_string_lossy();
                        if name != "target" && name != ".git" && name != ".agents" {
                            dirs.push(p);
                        }
                    } else if p.is_file() && p.extension().map_or(false, |ext| ext == "rs") {
                        if let Ok(content) = std::fs::read_to_string(&p) {
                            let url = url::Url::from_file_path(&p).unwrap();
                            let uri = DocumentUri::from_str(url.as_str()).unwrap();
                            for (line_idx, line) in content.lines().enumerate() {
                                if let Some(struct_name) = extract_name(line, "struct ") {
                                    let loc = make_location(&uri, line_idx, line, &struct_name);
                                    self.definitions.entry(struct_name).or_default().push(loc);
                                } else if let Some(fn_name) = extract_name(line, "fn ") {
                                    let loc = make_location(&uri, line_idx, line, &fn_name);
                                    self.definitions.entry(fn_name).or_default().push(loc);
                                } else if let Some(enum_name) = extract_name(line, "enum ") {
                                    let loc = make_location(&uri, line_idx, line, &enum_name);
                                    self.definitions.entry(enum_name).or_default().push(loc);
                                }

                                for word in tokenize(line) {
                                    let loc = make_location(&uri, line_idx, line, &word);
                                    self.references.entry(word).or_default().push(loc);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn tokenize(line: &str) -> Vec<String> {
    let mut words = Vec::new();
    let mut current = String::new();
    for c in line.chars() {
        if c.is_alphanumeric() || c == '_' {
            current.push(c);
        } else {
            if !current.is_empty() {
                words.push(current);
                current = String::new();
            }
        }
    }
    if !current.is_empty() {
        words.push(current);
    }
    words
}

fn extract_name(line: &str, keyword: &str) -> Option<String> {
    if let Some(idx) = line.find(keyword) {
        let name_start = idx + keyword.len();
        let name_part = &line[name_start..];
        let name = name_part.chars().take_while(|c| c.is_alphanumeric() || *c == '_').collect::<String>();
        if !name.is_empty() {
            return Some(name);
        }
    }
    None
}

fn make_location(uri: &DocumentUri, line: usize, line_content: &str, word: &str) -> Location {
    let col = line_content.find(word).unwrap_or(0);
    Location {
        uri: uri.clone(),
        range: Range {
            start: Position {
                line: line as u32,
                character: col as u32,
            },
            end: Position {
                line: line as u32,
                character: (col + word.len()) as u32,
            },
        },
    }
}

fn get_word_at_pos(path: &std::path::Path, pos: Position) -> Option<String> {
    let content = std::fs::read_to_string(path).ok()?;
    let line = content.lines().nth(pos.line as usize)?;
    let char_idx = pos.character as usize;
    if char_idx >= line.len() {
        return None;
    }
    let chars: Vec<char> = line.chars().collect();
    let mut start = char_idx;
    while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
        start -= 1;
    }
    let mut end = char_idx;
    while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
        end += 1;
    }
    if start == end {
        return None;
    }
    let word: String = chars[start..end].iter().collect();
    Some(word)
}

// --- STATIC GRAPH BACKEND ---

#[derive(Clone)]
struct StaticGraphBackend {
    client: tower_lsp_max::Client,
    index: Arc<std::sync::Mutex<SimpleWorkspaceIndex>>,
}

#[tower_lsp_max::async_trait]
impl tower_lsp_max::LanguageServer for StaticGraphBackend {
    async fn initialize(&self, _params: InitializeParams) -> tower_lsp_max::jsonrpc::Result<InitializeResult> {
        let mut caps = ServerCapabilities::default();
        caps.hover_provider = Some(HoverProviderCapability::Simple(true));
        caps.definition_provider = Some(OneOf::Left(true));
        caps.references_provider = Some(OneOf::Left(true));
        caps.text_document_sync = Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL));
        Ok(InitializeResult {
            capabilities: caps,
            server_info: Some(ServerInfo {
                name: "static-graph".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            offset_encoding: None,
        })
    }

    async fn initialized(&self, _params: InitializedParams) {}

    async fn shutdown(&self) -> tower_lsp_max::jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let diag = Diagnostic {
            range: Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: 0, character: 10 },
            },
            severity: Some(DiagnosticSeverity::WARNING),
            code: Some(NumberOrString::String("STATIC_001".to_string())),
            message: "Static graph analysis diagnostic".to_string(),
            ..Default::default()
        };
        self.client.publish_diagnostics(uri, vec![diag], None).await;
    }

    async fn hover(&self, params: HoverParams) -> tower_lsp_max::jsonrpc::Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        if let Ok(url) = url::Url::parse(uri.as_str()) {
            if let Ok(path) = url.to_file_path() {
                if let Some(word) = get_word_at_pos(&path, pos) {
                    let idx = self.index.lock().unwrap();
                    let defs_count = idx.definitions.get(&word).map_or(0, |v| v.len());
                    let refs_count = idx.references.get(&word).map_or(0, |v| v.len());
                    let hover_text = format!(
                        "### Symbol: {}\n- Definitions in index: {}\n- References in index: {}",
                        word, defs_count, refs_count
                    );
                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: hover_text,
                        }),
                        range: None,
                    }));
                }
            }
        }
        Ok(None)
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> tower_lsp_max::jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        if let Ok(url) = url::Url::parse(uri.as_str()) {
            if let Ok(path) = url.to_file_path() {
                if let Some(word) = get_word_at_pos(&path, pos) {
                    let idx = self.index.lock().unwrap();
                    if let Some(locs) = idx.definitions.get(&word) {
                        if !locs.is_empty() {
                            let response_locs: Vec<Location> = locs.iter().map(|loc| {
                                Location {
                                    uri: loc.uri.clone(),
                                    range: loc.range,
                                }
                            }).collect();
                            return Ok(Some(GotoDefinitionResponse::Array(response_locs)));
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> tower_lsp_max::jsonrpc::Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        if let Ok(url) = url::Url::parse(uri.as_str()) {
            if let Ok(path) = url.to_file_path() {
                if let Some(word) = get_word_at_pos(&path, pos) {
                    let idx = self.index.lock().unwrap();
                    if let Some(locs) = idx.references.get(&word) {
                        let response_locs: Vec<Location> = locs.iter().map(|loc| {
                            Location {
                                uri: loc.uri.clone(),
                                range: loc.range,
                            }
                        }).collect();
                        return Ok(Some(response_locs));
                    }
                }
            }
        }
        Ok(None)
    }
}

// --- MOCK PEER BACKEND ---

#[derive(Clone)]
struct MockPeerBackend {
    client: tower_lsp_max::Client,
    delay: Arc<std::sync::atomic::AtomicU64>,
}

#[tower_lsp_max::async_trait]
impl tower_lsp_max::LanguageServer for MockPeerBackend {
    async fn initialize(&self, _params: InitializeParams) -> tower_lsp_max::jsonrpc::Result<InitializeResult> {
        let mut caps = ServerCapabilities::default();
        caps.hover_provider = Some(HoverProviderCapability::Simple(true));
        caps.completion_provider = Some(CompletionOptions::default());
        caps.rename_provider = Some(OneOf::Left(true));
        caps.document_formatting_provider = Some(OneOf::Left(true));
        caps.code_action_provider = Some(CodeActionProviderCapability::Simple(true));
        caps.text_document_sync = Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL));
        Ok(InitializeResult {
            capabilities: caps,
            server_info: Some(ServerInfo {
                name: "mock-peer".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            offset_encoding: None,
        })
    }

    async fn initialized(&self, _params: InitializedParams) {}

    async fn shutdown(&self) -> tower_lsp_max::jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let diag = Diagnostic {
            range: Range {
                start: Position { line: 1, character: 0 },
                end: Position { line: 1, character: 10 },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("PEER_002".to_string())),
            message: "Mock peer dynamic diagnostic".to_string(),
            ..Default::default()
        };
        self.client.publish_diagnostics(uri, vec![diag], None).await;
    }

    async fn hover(&self, params: HoverParams) -> tower_lsp_max::jsonrpc::Result<Option<Hover>> {
        let delay_ms = self.delay.load(std::sync::atomic::Ordering::Relaxed);
        if delay_ms > 0 {
            tokio::time::sleep(std::time::Duration::from_millis(delay_ms)).await;
        }

        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let hover_text = if let Ok(url) = url::Url::parse(uri.as_str()) {
            if let Ok(path) = url.to_file_path() {
                if let Some(word) = get_word_at_pos(&path, pos) {
                    format!(
                        "### Dynamic Documentation for symbol: {}\nThis is served dynamically by `mock-peer`.",
                        word
                    )
                } else {
                    "### Hover from mock-peer".to_string()
                }
            } else {
                "### Hover from mock-peer".to_string()
            }
        } else {
            "### Hover from mock-peer".to_string()
        };

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: hover_text,
            }),
            range: None,
        }))
    }

    async fn completion(&self, _params: CompletionParams) -> tower_lsp_max::jsonrpc::Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem {
                label: "mock_completion_item".to_string(),
                ..Default::default()
            }
        ])))
    }

    async fn rename(&self, params: RenameParams) -> tower_lsp_max::jsonrpc::Result<Option<WorkspaceEdit>> {
        let delay_ms = self.delay.load(std::sync::atomic::Ordering::Relaxed);
        if delay_ms > 0 {
            tokio::time::sleep(std::time::Duration::from_millis(delay_ms)).await;
        }

        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let new_name = params.new_name;

        let version = if new_name.contains("stale") {
            Some(0)
        } else {
            Some(1)
        };

        let end_pos = Position {
            line: pos.line,
            character: pos.character + 7,
        };

        let doc_edit = TextDocumentEdit {
            text_document: OptionalVersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version,
            },
            edits: vec![
                OneOf3::A(TextEdit {
                    range: Range { start: pos, end: end_pos },
                    new_text: new_name,
                })
            ],
        };

        Ok(Some(WorkspaceEdit {
            changes: None,
            document_changes: Some(DocumentChanges::Edits(vec![doc_edit])),
            change_annotations: None,
            metadata: None,
        }))
    }

    async fn formatting(&self, _params: DocumentFormattingParams) -> tower_lsp_max::jsonrpc::Result<Option<Vec<TextEdit>>> {
        let pos = Position { line: 0, character: 0 };
        Ok(Some(vec![
            TextEdit {
                range: Range { start: pos, end: pos },
                new_text: "/* formatted by mock-peer */\n".to_string(),
            }
        ]))
    }

    async fn code_action(&self, _params: CodeActionParams) -> tower_lsp_max::jsonrpc::Result<Option<CodeActionResponse>> {
        let action = CodeAction {
            title: "mock-peer-code-action".to_string(),
            kind: Some(CodeActionKind::QUICKFIX),
            ..Default::default()
        };
        Ok(Some(vec![
            CodeActionOrCommand::CodeAction(action)
        ]))
    }
}

// --- TEST CLIENT HELPER ---

async fn read_message<R: tokio::io::AsyncRead + Unpin>(
    reader: &mut R,
) -> std::io::Result<serde_json::Value> {
    let mut header_buf = Vec::new();
    loop {
        let mut byte = [0u8; 1];
        reader.read_exact(&mut byte).await?;
        header_buf.push(byte[0]);
        if header_buf.ends_with(b"\r\n\r\n") {
            break;
        }
    }
    let header_str = String::from_utf8(header_buf)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
    let len_line = header_str
        .lines()
        .next()
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::InvalidData, "Empty header"))?;
    if !len_line.starts_with("Content-Length: ") {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Invalid Content-Length header",
        ));
    }
    let content_len: usize = len_line["Content-Length: ".len()..]
        .trim()
        .parse()
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

    let mut body = vec![0u8; content_len];
    reader.read_exact(&mut body).await?;
    let val = serde_json::from_slice(&body)?;
    Ok(val)
}

fn encode_message(msg: &serde_json::Value) -> Vec<u8> {
    let payload = serde_json::to_string(msg).unwrap();
    format!("Content-Length: {}\r\n\r\n{}", payload.len(), payload).into_bytes()
}

struct TestClient {
    pub stream: tokio::io::DuplexStream,
    next_id: i64,
    pub queued_messages: std::collections::VecDeque<Value>,
}

impl TestClient {
    pub fn new(stream: tokio::io::DuplexStream) -> Self {
        Self {
            stream,
            next_id: 1,
            queued_messages: std::collections::VecDeque::new(),
        }
    }

    pub async fn send_request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        let req = json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method,
            "params": params
        });
        let msg = encode_message(&req);
        self.stream.write_all(&msg).await.unwrap();

        loop {
            let mut idx = None;
            for (i, msg) in self.queued_messages.iter().enumerate() {
                if msg.get("id").and_then(|v| v.as_i64()) == Some(id) {
                    idx = Some(i);
                    break;
                }
            }
            if let Some(i) = idx {
                return self.queued_messages.remove(i).unwrap();
            }

            let resp = read_message(&mut self.stream).await.unwrap();
            if resp.get("id").and_then(|v| v.as_i64()) == Some(id) {
                return resp;
            } else {
                self.queued_messages.push_back(resp);
            }
        }
    }

    pub async fn send_notification(&mut self, method: &str, params: Value) {
        let req = json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params
        });
        let msg = encode_message(&req);
        self.stream.write_all(&msg).await.unwrap();
    }

    pub async fn read_queued_notification(&mut self, method: &str, timeout: std::time::Duration) -> Option<Value> {
        let start = std::time::Instant::now();
        loop {
            let mut idx = None;
            for (i, msg) in self.queued_messages.iter().enumerate() {
                if msg.get("method").and_then(|v| v.as_str()) == Some(method) {
                    idx = Some(i);
                    break;
                }
            }
            if let Some(i) = idx {
                return Some(self.queued_messages.remove(i).unwrap());
            }

            if start.elapsed() > timeout {
                return None;
            }

            let read_fut = read_message(&mut self.stream);
            match tokio::time::timeout(std::time::Duration::from_millis(50), read_fut).await {
                Ok(Ok(resp)) => {
                    if resp.get("method").and_then(|v| v.as_str()) == Some(method) {
                        return Some(resp);
                    } else {
                        self.queued_messages.push_back(resp);
                    }
                }
                _ => {}
            }
        }
    }
}

// --- UPSTREAM LAUNCHER ---

async fn start_upstream_server<S, F>(
    init: F,
) -> (String, tokio::task::JoinHandle<()>)
where
    S: tower_lsp_max::LanguageServer + 'static,
    F: FnOnce(tower_lsp_max::Client) -> S + Send + Sync + 'static,
{
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap().to_string();
    let handle = tokio::spawn(async move {
        let mut init_opt = Some(init);
        while let Ok((stream, _)) = listener.accept().await {
            let (reader, writer) = tokio::io::split(stream);
            let init_fn = init_opt.take().expect("Upstream server can only accept one connection in this harness");
            let (service, socket) = tower_lsp_max::LspService::new(init_fn);
            let _ = tower_lsp_max::Server::new(reader, writer, socket).serve(service).await;
        }
    });
    (addr, handle)
}

// --- MAIN RUNNER ---

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .init();

    println!("======================================================================");
    println!("R8 - Playground/ Composite LSP Dogfood Harness");
    println!("======================================================================");

    // 1. Scan and index workspaces
    println!("Step 1: Scanning and indexing real local workspaces...");
    let mut index = SimpleWorkspaceIndex::new();
    index.index_dir("/Users/sac/tower-lsp-max");
    index.index_dir("/Users/sac/lsp-types-max");
    let index_arc = Arc::new(std::sync::Mutex::new(index));
    let index_clone_1 = index_arc.clone();
    println!("Workspace indexing complete.");

    // 2. Start mock-peer and static-graph upstream servers on TCP
    println!("Step 2: Spawning upstream servers...");
    let (static_graph_addr, static_graph_task) = start_upstream_server(move |client| {
        StaticGraphBackend {
            client,
            index: index_clone_1,
        }
    }).await;
    println!("-> Static graph upstream server started on: {}", static_graph_addr);

    let mock_delay = Arc::new(std::sync::atomic::AtomicU64::new(0));
    let mock_delay_clone = mock_delay.clone();
    let (mock_peer_addr, mock_peer_task) = start_upstream_server(move |client| {
        MockPeerBackend {
            client,
            delay: mock_delay_clone,
        }
    }).await;
    println!("-> Mock peer upstream server started on: {}", mock_peer_addr);

    // 3. Initialize ComposedServer
    println!("Step 3: Initializing ComposedServer...");
    let upstreams = vec![
        ("static-graph".to_string(), static_graph_addr.clone()),
        ("mock-peer".to_string(), mock_peer_addr.clone()),
    ];

    let (client_io, server_io) = tokio::io::duplex(1024 * 1024);
    
    // We will extract the composed server state by wrapping the server
    let composed_state_holder = Arc::new(std::sync::Mutex::new(None));
    let composed_state_holder_clone = composed_state_holder.clone();

    let (service, socket) = tower_lsp_max::LspService::new(|client| {
        let server = ComposedServer::new(client, upstreams.clone());
        let composed_state = server.state().clone();
        let mut holder = composed_state_holder_clone.lock().unwrap();
        *holder = Some(composed_state);
        server
    });

    // Spawn composed server
    let (server_reader, server_writer) = tokio::io::split(server_io);
    let composed_server_task = tokio::spawn(async move {
        let _ = tower_lsp_max::Server::new(server_reader, server_writer, socket).serve(service).await;
    });

    let mut client = TestClient::new(client_io);

    // Extract the state pointer set in the factory closure
    let composed_state = {
        let mut holder = composed_state_holder.lock().unwrap();
        holder.take().expect("ComposedServer state not captured during construction")
    };

    // Set registry root path
    {
        let mut reg = tower_lsp_max::get_registry().lock().unwrap();
        reg.root_path = std::path::PathBuf::from("/Users/sac/tower-lsp-max");
        println!("Set ServerRegistry root_path to: {:?}", reg.root_path);
    }

    // 4. Send initialize and initialized
    println!("Step 4: Executing initialize handshake...");
    let init_resp = client.send_request("initialize", json!({
        "capabilities": {
            "textDocument": {
                "hover": { "contentFormat": ["markdown"] }
            }
        },
        "rootUri": "file:///Users/sac/tower-lsp-max",
        "processId": null
    })).await;
    assert!(init_resp.get("result").is_some());
    println!("-> Initialize handshake succeeded.");

    client.send_notification("initialized", json!({})).await;

    // 5. Open a document containing real workspace contents
    let real_file_uri = "file:///Users/sac/tower-lsp-max/crates/playground/src/lib.rs";
    println!("Step 5: Opening file: {}", real_file_uri);
    client.send_notification("textDocument/didOpen", json!({
        "textDocument": {
            "uri": real_file_uri,
            "languageId": "rust",
            "version": 1,
            "text": std::fs::read_to_string("/Users/sac/tower-lsp-max/crates/playground/src/lib.rs").unwrap()
        }
    })).await;

    // 6. Verify diagnostics merging with attribution metadata
    println!("Step 6: Verifying publishDiagnostics merge with source attributions...");
    let diag_notification = client.read_queued_notification("textDocument/publishDiagnostics", std::time::Duration::from_secs(3))
        .await
        .expect("Timeout waiting for publishDiagnostics");
    let diags = diag_notification.get("params").unwrap().get("diagnostics").unwrap().as_array().unwrap();
    println!("-> Received {} diagnostics.", diags.len());
    for diag in diags {
        let source = diag.get("source").unwrap().as_str().unwrap();
        let message = diag.get("message").unwrap().as_str().unwrap();
        let code = diag.get("code").unwrap().get("value").or(diag.get("code")).unwrap().as_str().unwrap();
        println!("  * Source: [{}], Code: [{}], Message: [{}]", source, code, message);
        assert!(source == "static-graph" || source == "mock-peer");
    }

    // 7. Verify read-only paths (hover, definition, references)
    let file_content = std::fs::read_to_string("/Users/sac/tower-lsp-max/crates/playground/src/lib.rs").unwrap();
    let mut hover_line = 27;
    let mut hover_col = 15;
    for (line_idx, line) in file_content.lines().enumerate() {
        if line.contains("pub struct Backend") {
            hover_line = line_idx;
            hover_col = line.find("Backend").unwrap();
            break;
        }
    }

    println!("Step 7: Querying textDocument/hover on symbol 'Backend' (line {}, col {})...", hover_line, hover_col);
    let hover_resp = client.send_request("textDocument/hover", json!({
        "textDocument": { "uri": real_file_uri },
        "position": { "line": hover_line, "character": hover_col }
    })).await;
    let hover_result = hover_resp.get("result").unwrap();
    println!("-> Hover result contents: {:?}", hover_result.get("contents").unwrap().get("value").unwrap().as_str().unwrap());
    assert!(hover_result.get("contents").unwrap().get("value").unwrap().as_str().unwrap().contains("Source: static-graph"));
    assert!(hover_result.get("contents").unwrap().get("value").unwrap().as_str().unwrap().contains("Source: mock-peer"));

    println!("Step 7b: Querying textDocument/definition on 'Backend'...");
    let def_resp = client.send_request("textDocument/definition", json!({
        "textDocument": { "uri": real_file_uri },
        "position": { "line": hover_line, "character": hover_col }
    })).await;
    println!("-> Definition locations: {:?}", def_resp.get("result").unwrap());
    assert!(!def_resp.get("result").unwrap().is_null());

    println!("Step 7c: Querying textDocument/references on 'Backend'...");
    let ref_resp = client.send_request("textDocument/references", json!({
        "textDocument": { "uri": real_file_uri },
        "position": { "line": hover_line, "character": hover_col },
        "context": { "includeDeclaration": true }
    })).await;
    println!("-> References count found: {}", ref_resp.get("result").unwrap().as_array().unwrap().len());
    assert!(!ref_resp.get("result").unwrap().as_array().unwrap().is_empty());

    // 8. Query codeAction
    println!("Step 8: Querying textDocument/codeAction...");
    let action_resp = client.send_request("textDocument/codeAction", json!({
        "textDocument": { "uri": real_file_uri },
        "range": {
            "start": { "line": hover_line, "character": hover_col },
            "end": { "line": hover_line, "character": hover_col }
        },
        "context": {
            "diagnostics": []
        }
    })).await;
    println!("-> Code action result: {:?}", action_resp.get("result").unwrap());
    assert!(!action_resp.get("result").unwrap().as_array().unwrap().is_empty());

    // 9. Query mutations through TransactionEditGate
    println!("Step 9a: Querying textDocument/rename (valid version 1)...");
    let rename_resp = client.send_request("textDocument/rename", json!({
        "textDocument": { "uri": real_file_uri },
        "position": { "line": hover_line, "character": hover_col },
        "newName": "Backend_New"
    })).await;
    println!("-> Rename result: {:?}", rename_resp.get("result").unwrap());
    assert!(rename_resp.get("error").is_none());

    {
        let mut s = composed_state.lock().await;
        s.edit_gate.clear_for_uri(real_file_uri);
    }

    println!("Step 9b: Querying textDocument/rename with stale edit (version 0)...");
    let stale_rename_resp = client.send_request("textDocument/rename", json!({
        "textDocument": { "uri": real_file_uri },
        "position": { "line": hover_line, "character": hover_col },
        "newName": "Backend_stale"
    })).await;
    println!("-> Rename result (stale): {:?}", stale_rename_resp.get("error").unwrap());
    assert!(stale_rename_resp.get("error").is_some());
    assert!(stale_rename_resp.get("error").unwrap().get("message").unwrap().as_str().unwrap().contains("Stale"));

    {
        let mut s = composed_state.lock().await;
        s.edit_gate.clear_for_uri(real_file_uri);
    }

    println!("Step 9c: Querying textDocument/rename twice to test overlapping edits...");
    let rename_resp1 = client.send_request("textDocument/rename", json!({
        "textDocument": { "uri": real_file_uri },
        "position": { "line": hover_line, "character": hover_col },
        "newName": "Backend_First"
    })).await;
    assert!(rename_resp1.get("error").is_none());

    let rename_resp2 = client.send_request("textDocument/rename", json!({
        "textDocument": { "uri": real_file_uri },
        "position": { "line": hover_line, "character": hover_col },
        "newName": "Backend_Second"
    })).await;
    println!("-> Second rename result (overlapping): {:?}", rename_resp2.get("error").unwrap());
    assert!(rename_resp2.get("error").is_some());
    assert!(rename_resp2.get("error").unwrap().get("message").unwrap().as_str().unwrap().contains("Overlapping"));

    {
        let mut s = composed_state.lock().await;
        s.edit_gate.clear_for_uri(real_file_uri);
    }

    // 10. Verify SourceDegraded rejection
    println!("Step 10: Degrading mock-peer source health and querying rename...");
    mock_delay.store(120, std::sync::atomic::Ordering::Relaxed);

    let rename_req_id = 999;
    let rename_req = json!({
        "jsonrpc": "2.0",
        "id": rename_req_id,
        "method": "textDocument/rename",
        "params": {
            "textDocument": { "uri": real_file_uri },
            "position": { "line": hover_line, "character": hover_col },
            "newName": "Backend_Degraded"
        }
    });

    let msg = encode_message(&rename_req);
    client.stream.write_all(&msg).await.unwrap();

    tokio::time::sleep(std::time::Duration::from_millis(30)).await;
    println!("  * Removing mock-peer from capability tracker while rename is in flight...");
    let mock_peer_backup = {
        let mut s = composed_state.lock().await;
        s.capability_tracker.sources.remove("mock-peer").unwrap()
    };

    let degraded_rename_resp = client.read_response(rename_req_id).await;
    println!("-> Rename result (degraded source) raw: {:?}", degraded_rename_resp);
    println!("-> Rename result (degraded source) error: {:?}", degraded_rename_resp.get("error"));
    assert!(degraded_rename_resp.get("error").unwrap().get("message").unwrap().as_str().unwrap().contains("SourceDegraded"));

    {
        let mut s = composed_state.lock().await;
        s.capability_tracker.sources.insert("mock-peer".to_string(), mock_peer_backup);
        s.edit_gate.clear_for_uri(real_file_uri);
    }
    mock_delay.store(0, std::sync::atomic::Ordering::Relaxed);

    // 11. Verify stale-response refusal (version increment during slow query)
    println!("Step 11: Verifying stale-response refusal during slow query...");
    mock_delay.store(120, std::sync::atomic::Ordering::Relaxed);

    let hover_req_id = 889;
    let hover_req = json!({
        "jsonrpc": "2.0",
        "id": hover_req_id,
        "method": "textDocument/hover",
        "params": {
            "textDocument": { "uri": real_file_uri },
            "position": { "line": hover_line, "character": hover_col }
        }
    });

    let msg = encode_message(&hover_req);
    client.stream.write_all(&msg).await.unwrap();

    tokio::time::sleep(std::time::Duration::from_millis(30)).await;
    println!("  * Bumping document version to 2 while hover is in flight...");
    client.send_notification("textDocument/didChange", json!({
        "textDocument": {
            "uri": real_file_uri,
            "version": 2
        },
        "contentChanges": [
            { "text": file_content.clone() }
        ]
    })).await;

    let hover_slow_resp = client.read_response(hover_req_id).await;
    println!("-> Slow hover response (after version change): {:?}", hover_slow_resp);
    assert!(hover_slow_resp.get("result").unwrap().is_null());

    mock_delay.store(0, std::sync::atomic::Ordering::Relaxed);

    // 12. Retrieve traces and write JSONL receipt
    println!("Step 12: Generating JSONL transcript...");
    let request_traces_arc = {
        let s = composed_state.lock().await;
        s.request_traces.clone()
    };
    let traces = request_traces_arc.lock().unwrap().clone();

    println!("Total recorded request traces: {}", traces.len());
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();

    let root_receipts_dir = std::path::Path::new("/Users/sac/tower-lsp-max/playground/receipts");
    let crate_receipts_dir = std::path::Path::new("/Users/sac/tower-lsp-max/crates/playground/receipts");
    std::fs::create_dir_all(root_receipts_dir).ok();
    std::fs::create_dir_all(crate_receipts_dir).ok();

    let filename = format!("{}.jsonl", timestamp);
    let root_file_path = root_receipts_dir.join(&filename);
    let crate_file_path = crate_receipts_dir.join(&filename);

    let mut lines = Vec::new();
    for trace in &traces {
        let line = serde_json::to_string(trace).unwrap();
        println!("{}", line);
        lines.push(line);
    }
    let content = lines.join("\n") + "\n";

    std::fs::write(&root_file_path, &content).unwrap();
    std::fs::write(&crate_file_path, &content).unwrap();
    println!("-> Emitted transcript into: {:?}", root_file_path);

    // Clean shutdown
    println!("Step 13: Cleaning up...");
    let shutdown_resp = client.send_request("shutdown", json!(null)).await;
    assert!(shutdown_resp.get("result").is_some());
    client.send_notification("exit", json!(null)).await;

    composed_server_task.abort();
    static_graph_task.abort();
    mock_peer_task.abort();

    println!("======================================================================");
    println!("All verification scenarios successfully completed and verified!");
    println!("======================================================================");
}

impl TestClient {
    pub async fn read_response(&mut self, id: i64) -> Value {
        loop {
            let mut idx = None;
            for (i, msg) in self.queued_messages.iter().enumerate() {
                if msg.get("id").and_then(|v| v.as_i64()) == Some(id) {
                    idx = Some(i);
                    break;
                }
            }
            if let Some(i) = idx {
                return self.queued_messages.remove(i).unwrap();
            }

            let resp = read_message(&mut self.stream).await.unwrap();
            if resp.get("id").and_then(|v| v.as_i64()) == Some(id) {
                return resp;
            } else {
                self.queued_messages.push_back(resp);
            }
        }
    }
}
