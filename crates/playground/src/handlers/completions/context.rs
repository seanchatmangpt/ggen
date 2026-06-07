use tower_lsp_max::lsp_types_max::Position;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CompletionContext {
    ImplLanguageServerMethod { partial: String },
    ServerCapabilitiesField { partial: String },
    RpcName { partial: String },
    LspServiceBuilder,
    MessageTypeVariant,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BlockType {
    ImplLanguageServer,
    ServerCapabilities,
    Other,
}

#[allow(clippy::needless_range_loop)]
pub(crate) fn detect_context(source: &str, pos: Position) -> CompletionContext {
    let lines: Vec<&str> = source.lines().collect();
    if lines.is_empty() {
        return CompletionContext::Unknown;
    }
    let safe_line_idx = (pos.line as usize).min(lines.len() - 1);
    let current_line = lines[safe_line_idx];
    let char_pos = (pos.character as usize).min(current_line.len());
    let prefix_str = current_line.chars().take(char_pos).collect::<String>();
    let prefix = &prefix_str;

    if let Some(partial) = find_rpc_attribute_partial(prefix, &lines, safe_line_idx) {
        return CompletionContext::RpcName { partial };
    }

    // MessageType:: context
    if prefix.contains("MessageType::") {
        return CompletionContext::MessageTypeVariant;
    }

    // LspService builder: line starts with `.` after LspService::build chain
    if prefix.trim_start().starts_with('.') {
        for line in lines[..safe_line_idx].iter().rev().take(10) {
            if line.contains("LspService::build") {
                return CompletionContext::LspServiceBuilder;
            }
        }
    }

    // Construct chars list of everything up to the cursor to run brace-depth scanning
    let mut chars = Vec::new();
    for i in 0..=safe_line_idx {
        let line = if i == safe_line_idx { prefix } else { lines[i] };
        for c in line.chars() {
            chars.push(c);
        }
        if i < safe_line_idx {
            chars.push('\n');
        }
    }

    let mut stack = Vec::new();
    let mut pending = None;

    enum ScanState {
        Normal,
        LineComment,
        BlockComment { depth: usize },
        StringLiteral { escaped: bool },
        CharLiteral { escaped: bool },
        RawStringLiteral { hash_count: usize },
    }

    let mut state = ScanState::Normal;
    let mut idx = 0;

    let matches_at = |chars: &[char], idx: usize, pattern: &str| -> bool {
        if idx + pattern.len() > chars.len() {
            return false;
        }
        let sub = &chars[idx..idx + pattern.len()];
        sub.iter().copied().eq(pattern.chars())
    };

    while idx < chars.len() {
        match &mut state {
            ScanState::Normal => {
                if matches_at(&chars, idx, "//") {
                    state = ScanState::LineComment;
                    idx += 2;
                } else if matches_at(&chars, idx, "/*") {
                    state = ScanState::BlockComment { depth: 1 };
                    idx += 2;
                } else if chars[idx] == '"' {
                    state = ScanState::StringLiteral { escaped: false };
                    idx += 1;
                } else if chars[idx] == '\'' {
                    state = ScanState::CharLiteral { escaped: false };
                    idx += 1;
                } else if chars[idx] == 'r' {
                    // Check if it's a raw string literal
                    let mut temp = idx + 1;
                    let mut hash_count = 0;
                    while temp < chars.len() && chars[temp] == '#' {
                        hash_count += 1;
                        temp += 1;
                    }
                    if temp < chars.len() && chars[temp] == '"' {
                        state = ScanState::RawStringLiteral { hash_count };
                        idx = temp + 1;
                    } else {
                        idx += 1;
                    }
                } else if matches_at(&chars, idx, "impl LanguageServer") {
                    pending = Some(BlockType::ImplLanguageServer);
                    idx += "impl LanguageServer".len();
                } else if matches_at(&chars, idx, "ServerCapabilities") {
                    pending = Some(BlockType::ServerCapabilities);
                    idx += "ServerCapabilities".len();
                } else {
                    let c = chars[idx];
                    match c {
                        '{' => {
                            stack.push(pending.take().unwrap_or(BlockType::Other));
                        }
                        '}' => {
                            stack.pop();
                            pending = None;
                        }
                        ';' => {
                            pending = None;
                        }
                        _ => {}
                    }
                    idx += 1;
                }
            }
            ScanState::LineComment => {
                if chars[idx] == '\n' {
                    state = ScanState::Normal;
                }
                idx += 1;
            }
            ScanState::BlockComment { depth } => {
                if matches_at(&chars, idx, "/*") {
                    *depth += 1;
                    idx += 2;
                } else if matches_at(&chars, idx, "*/") {
                    *depth -= 1;
                    if *depth == 0 {
                        state = ScanState::Normal;
                    }
                    idx += 2;
                } else {
                    idx += 1;
                }
            }
            ScanState::StringLiteral { escaped } => {
                if *escaped {
                    *escaped = false;
                    idx += 1;
                } else if chars[idx] == '\\' {
                    *escaped = true;
                    idx += 1;
                } else if chars[idx] == '"' {
                    state = ScanState::Normal;
                    idx += 1;
                } else {
                    idx += 1;
                }
            }
            ScanState::CharLiteral { escaped } => {
                if *escaped {
                    *escaped = false;
                    idx += 1;
                } else if chars[idx] == '\\' {
                    *escaped = true;
                    idx += 1;
                } else if chars[idx] == '\'' {
                    state = ScanState::Normal;
                    idx += 1;
                } else {
                    idx += 1;
                }
            }
            ScanState::RawStringLiteral { hash_count } => {
                if chars[idx] == '"' {
                    let mut matched = true;
                    for h in 0..*hash_count {
                        if idx + 1 + h >= chars.len() || chars[idx + 1 + h] != '#' {
                            matched = false;
                            break;
                        }
                    }
                    if matched {
                        idx += 1 + *hash_count;
                        state = ScanState::Normal;
                    } else {
                        idx += 1;
                    }
                } else {
                    idx += 1;
                }
            }
        }
    }

    let mut active_context = CompletionContext::Unknown;
    let mut can_match_caps = true;
    for block in stack.iter().rev() {
        match block {
            BlockType::ServerCapabilities => {
                if can_match_caps {
                    let partial = prefix
                        .split(|c: char| !c.is_alphanumeric() && c != '_')
                        .next_back()
                        .unwrap_or("")
                        .to_string();
                    active_context = CompletionContext::ServerCapabilitiesField { partial };
                    break;
                }
            }
            BlockType::ImplLanguageServer => {
                let partial = prefix
                    .split(|c: char| !c.is_alphanumeric() && c != '_' && c != '/' && c != '$')
                    .next_back()
                    .unwrap_or("")
                    .to_string();
                active_context = CompletionContext::ImplLanguageServerMethod { partial };
                break;
            }
            BlockType::Other => {
                can_match_caps = false;
            }
        }
    }

    active_context
}

fn find_rpc_attribute_partial(
    prefix: &str,
    lines: &[&str],
    current_line_idx: usize,
) -> Option<String> {
    if let Some(pos) = find_rpc_in_line(prefix) {
        return Some(pos);
    }
    if let Some(partial) = find_name_literal_in_line(prefix) {
        // scan upward up to 3 lines
        for i in (0..current_line_idx).rev().take(3) {
            let line = lines[i];
            if line.contains("fn ")
                || line.contains("struct ")
                || line.contains("impl ")
                || line.contains("trait ")
            {
                break;
            }
            if line.contains("#[rpc") || line.contains("rpc") {
                return Some(partial);
            }
        }
    }
    None
}

fn find_name_in_rpc_params(params: &str) -> Option<String> {
    let bytes = params.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i..].starts_with(b"name") {
            let preceded_ok =
                i == 0 || !bytes[i - 1].is_ascii_alphanumeric() && bytes[i - 1] != b'_';
            if preceded_ok {
                let mut j = i + 4;
                while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                    j += 1;
                }
                if j < bytes.len() && bytes[j] == b'=' {
                    j += 1;
                    while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                        j += 1;
                    }
                    if j < bytes.len() && bytes[j] == b'"' {
                        j += 1;
                        let content_part = &params[j..];
                        if let Some(quote_idx) = content_part.find('"') {
                            return Some(content_part[..quote_idx].to_string());
                        } else {
                            return Some(content_part.to_string());
                        }
                    }
                }
            }
        }
        i += 1;
    }
    None
}

fn find_rpc_in_line(prefix: &str) -> Option<String> {
    let bytes = prefix.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i..].starts_with(b"rpc") {
            let preceded_ok = i == 0
                || !bytes[i - 1].is_ascii_alphanumeric()
                    && bytes[i - 1] != b'_'
                    && bytes[i - 1] != b'#';
            if preceded_ok {
                let mut j = i + 3;
                while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                    j += 1;
                }
                if j < bytes.len() && bytes[j] == b'(' {
                    j += 1;
                    if let Some(name) = find_name_in_rpc_params(&prefix[j..]) {
                        return Some(name);
                    }
                }
            }
        }
        i += 1;
    }
    None
}

fn find_name_literal_in_line(prefix: &str) -> Option<String> {
    let bytes = prefix.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i..].starts_with(b"name") {
            let preceded_ok =
                i == 0 || !bytes[i - 1].is_ascii_alphanumeric() && bytes[i - 1] != b'_';
            if preceded_ok {
                let mut j = i + 4;
                while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                    j += 1;
                }
                if j < bytes.len() && bytes[j] == b'=' {
                    j += 1;
                    while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                        j += 1;
                    }
                    if j < bytes.len() && bytes[j] == b'"' {
                        j += 1;
                        let content_part = &prefix[j..];
                        if let Some(quote_idx) = content_part.find('"') {
                            return Some(content_part[..quote_idx].to_string());
                        } else {
                            return Some(content_part.to_string());
                        }
                    }
                }
            }
        }
        i += 1;
    }
    None
}
