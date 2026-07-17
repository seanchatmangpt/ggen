// Simple TOML parsing for hook pack metadata

/// Parse a minimal TOML document for HookPack metadata.
/// Expected format:
/// ```text
/// [pack]
/// name = "pack name"
/// version = "1.0.0"
/// description = "pack description"
/// required_dialects = ["dialect1", "dialect2"]
/// ```
pub fn parse_simple_toml(content: &str) -> Result<(String, String, String, Vec<String>), String> {
    let mut name = None;
    let mut version = None;
    let mut description = None;
    let mut required_dialects = Vec::new();
    let mut in_pack_section = false;

    for (line_idx, line) in content.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if line.starts_with('[') && line.ends_with(']') {
            let section = line[1..line.len() - 1].trim();
            if section == "pack" {
                in_pack_section = true;
            } else {
                return Err(format!("unknown TOML section: {}", section));
            }
            continue;
        }
        if !in_pack_section {
            continue;
        }
        let Some((key, val)) = line.split_once('=') else {
            return Err(format!("invalid TOML line {}: {}", line_idx + 1, line));
        };
        let key = key.trim();
        let val = val.trim();
        match key {
            "name" => {
                name = Some(strip_quotes(val)?);
            }
            "version" => {
                version = Some(strip_quotes(val)?);
            }
            "description" => {
                description = Some(strip_quotes(val)?);
            }
            "required_dialects" => {
                required_dialects = parse_toml_array(val)?;
            }
            other => {
                return Err(format!("unknown TOML key: {}", other));
            }
        }
    }

    let name = name.ok_or_else(|| "missing hook pack name".to_string())?;
    let version = version.ok_or_else(|| "missing hook pack version".to_string())?;
    let description = description.ok_or_else(|| "missing hook pack description".to_string())?;

    Ok((name, version, description, required_dialects))
}

fn strip_quotes(s: &str) -> Result<String, String> {
    if (s.starts_with('"') && s.ends_with('"')) || (s.starts_with('\'') && s.ends_with('\'')) {
        Ok(s[1..s.len() - 1].to_string())
    } else {
        Err(format!("expected quoted string literal, got: {}", s))
    }
}

fn parse_toml_array(s: &str) -> Result<Vec<String>, String> {
    if !s.starts_with('[') || !s.ends_with(']') {
        return Err(format!("expected TOML array, got: {}", s));
    }
    let inner = &s[1..s.len() - 1];
    let mut out = Vec::new();
    for item in inner.split(',') {
        let item = item.trim();
        if item.is_empty() {
            continue;
        }
        out.push(strip_quotes(item)?);
    }
    Ok(out)
}
