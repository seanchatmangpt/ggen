// Datalog to N3 translation utilities

struct DatalogAtom {
    name: String,
    args: Vec<String>,
}

pub(crate) fn split_depth0(text: &str, sep: char) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (i, c) in text.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            c if c == sep && depth == 0 => {
                parts.push(&text[start..i]);
                start = i + c.len_utf8();
            }
            _ => {}
        }
    }
    parts.push(&text[start..]);
    parts
}

fn parse_datalog_atom(s: &str, _subject: &str) -> Result<DatalogAtom, String> {
    let s = s.trim();
    let open = s
        .find('(')
        .ok_or_else(|| format!("datalog atom '{s}' missing '('"))?;
    if !s.ends_with(')') {
        return Err(format!("datalog atom '{s}' missing ')'"));
    }
    let name = s[..open].trim().to_string();
    if name.is_empty() {
        return Err(format!("datalog atom '{s}' has empty predicate"));
    }
    let inner = &s[open + 1..s.len() - 1];
    let args: Vec<String> = if inner.trim().is_empty() {
        Vec::new()
    } else {
        inner.split(',').map(|t| t.trim().to_string()).collect()
    };
    Ok(DatalogAtom { name, args })
}

fn format_term(s: &str) -> String {
    let s = s.trim();
    if s.starts_with('?') {
        let rest = &s[1..];
        if !rest.is_empty() && rest.chars().all(|c| c.is_ascii_digit()) {
            format!("?v{}", rest)
        } else {
            s.to_string()
        }
    } else if s.starts_with('<') && s.ends_with('>') {
        s.to_string()
    } else if s.starts_with('"') && s.ends_with('"') {
        s.to_string()
    } else if s.chars().all(|c| c.is_ascii_digit()) {
        format!("\"{}\"^^<http://www.w3.org/2001/XMLSchema#integer>", s)
    } else {
        format!("<{}>", s)
    }
}

pub fn translate_datalog_to_n3(program: &str, subject: &str) -> Result<String, String> {
    let mut n3_rules = String::new();
    let statements = split_depth0(program, '.');
    let mut added = 0;
    for stmt in statements {
        let stmt = stmt.trim();
        if stmt.is_empty() {
            continue;
        }
        let (head_s, body_s) = match stmt.split_once(":-") {
            Some((h, b)) => (h, Some(b)),
            None => (stmt, None),
        };
        let head_atom = parse_datalog_atom(head_s, subject)?;
        if head_atom.name == "t" {
            return Err("datalog head predicate 't' is reserved for EDB".to_string());
        }
        let mut body_triples = Vec::new();
        if let Some(b) = body_s {
            for lit in split_depth0(b, ',') {
                let lit = lit.trim();
                if lit.is_empty() {
                    continue;
                }
                let (negated, atom_str) = if let Some(stripped) = lit.strip_prefix('!') {
                    (true, stripped.trim())
                } else {
                    (false, lit)
                };
                let atom = parse_datalog_atom(atom_str, subject)?;
                let triple_str = if atom.name == "t" {
                    if atom.args.len() != 3 {
                        return Err(format!("t atom must have arity 3, got {}", atom.args.len()));
                    }
                    format!(
                        "{} {} {}",
                        format_term(&atom.args[0]),
                        format_term(&atom.args[1]),
                        format_term(&atom.args[2])
                    )
                } else {
                    match atom.args.len() {
                        1 => format!(
                            "{} <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <{}>",
                            format_term(&atom.args[0]),
                            atom.name
                        ),
                        2 => format!(
                            "{} <{}> {}",
                            format_term(&atom.args[0]),
                            atom.name,
                            format_term(&atom.args[1])
                        ),
                        _ => {
                            return Err(format!(
                                "atom '{}' must have arity 1 or 2, got {}",
                                atom.name,
                                atom.args.len()
                            ))
                        }
                    }
                };
                if negated {
                    body_triples.push(format!("not {{ {} }}", triple_str));
                } else {
                    body_triples.push(triple_str);
                }
            }
        }
        let head_triple_str = match head_atom.args.len() {
            1 => format!(
                "{} <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <{}>",
                format_term(&head_atom.args[0]),
                head_atom.name
            ),
            2 => format!(
                "{} <{}> {}",
                format_term(&head_atom.args[0]),
                head_atom.name,
                format_term(&head_atom.args[1])
            ),
            _ => {
                return Err(format!(
                    "head atom '{}' must have arity 1 or 2, got {}",
                    head_atom.name,
                    head_atom.args.len()
                ))
            }
        };
        n3_rules.push_str(&format!(
            "{{ {} }} => {{ {} }} .\n",
            body_triples.join(" . "),
            head_triple_str
        ));
        added += 1;
        if added > 8 {
            return Err("more than 8 rules in datalog program".to_string());
        }
    }
    Ok(n3_rules)
}
