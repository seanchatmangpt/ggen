use crate::models::{Construct8Packet, SymbolTable};
use blake3::Hasher;

/// Custom error representing a canonical byte forge failure.
#[derive(Debug, thiserror::Error)]
pub enum ForgeError {
    #[error("Symbol handle {0} not found in symbol table")]
    SymbolNotFound(u32),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Forges canonical bytes (e.g., N-Quads) from a Construct8Packet and updates the receipt hasher.
pub fn forge_nquads(
    packet: &Construct8Packet,
    symbols: &SymbolTable,
    hasher: &mut Hasher,
    out: &mut dyn std::io::Write,
) -> std::io::Result<usize> {
    let mut written = 0;

    if packet.emit_mask != 0 {
        let seed_hex: String = packet
            .receipt_seed
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect();
        let header = format!(
            "# epoch: {}\n# law_ref: {}\n# receipt_seed: {}\n",
            packet.epoch, packet.law_ref, seed_hex
        );
        out.write_all(header.as_bytes())?;
        hasher.update(header.as_bytes());
    }

    // N-Quads are canonicalized by ordering within the packet, but for M1 we just emit based on emit_mask
    for i in 0..8 {
        if (packet.emit_mask & (1 << i)) != 0 {
            let s = symbols
                .lookup(packet.subjects[i])
                .unwrap_or_else(|| "_:unknown".to_string());
            let p = symbols
                .lookup(packet.predicates[i])
                .unwrap_or_else(|| "_:unknown".to_string());
            let o = symbols
                .lookup(packet.objects[i])
                .unwrap_or_else(|| "_:unknown".to_string());

            // Format: <S> <P> <O> .
            // In a real N-Quads forge, literals and types would be properly escaped and identified by kind_mask.
            // For MVP, we assume IRIs are properly formatted in the symbol table (e.g., `<http://...>` or `"literal"`).
            let line = format!("{} {} {} .\n", s, p, o);
            let bytes = line.as_bytes();

            out.write_all(bytes)?;
            hasher.update(bytes);
            written += 1;
        }
    }

    Ok(written)
}

/// Formats the packet stream into a canonical, deterministic byte representation
/// (lexicographically sorted N-Quads) and computes its BLAKE3 hash as the transaction receipt.
pub fn forge_canonical(
    packets: &[Construct8Packet],
    symbols: &SymbolTable,
) -> Result<(Vec<u8>, blake3::Hash), ForgeError> {
    let mut nquads = Vec::new();

    for packet in packets {
        let seed_hex: String = packet
            .receipt_seed
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect();
        let prefix = format!(
            "# epoch: {}\n# law_ref: {}\n# receipt_seed: {}\n",
            packet.epoch, packet.law_ref, seed_hex
        );

        for i in 0..8 {
            if (packet.emit_mask & (1 << i)) != 0 {
                let s_id = packet.subjects[i];
                let p_id = packet.predicates[i];
                let o_id = packet.objects[i];

                let s = symbols
                    .lookup(s_id)
                    .ok_or(ForgeError::SymbolNotFound(s_id))?;
                let p = symbols
                    .lookup(p_id)
                    .ok_or(ForgeError::SymbolNotFound(p_id))?;
                let o = symbols
                    .lookup(o_id)
                    .ok_or(ForgeError::SymbolNotFound(o_id))?;

                // Canonicalize terms:
                // Subject: must be IRI (<...>) or Blank Node (_:...)
                let s_formatted = if s.starts_with('_') || s.starts_with('<') {
                    s.clone()
                } else {
                    format!("<{}>", s)
                };

                // Predicate: must be IRI (<...>)
                let p_formatted = if p.starts_with('<') {
                    p.clone()
                } else {
                    format!("<{}>", p)
                };

                // Object: can be IRI (<...>), Blank Node (_:...), or Literal ("...")
                let o_formatted = if o.starts_with('_') || o.starts_with('<') || o.starts_with('"')
                {
                    o.clone()
                } else {
                    // Smart wrap as IRI or Literal
                    if o.starts_with("http://") || o.starts_with("https://") {
                        format!("<{}>", o)
                    } else {
                        format!("\"{}\"", o.replace('\\', "\\\\").replace('"', "\\\""))
                    }
                };

                let line = format!("{} {} {} .\n", s_formatted, p_formatted, o_formatted);
                nquads.push(format!("{}{}", prefix, line));
            }
        }
    }

    // Sort lexicographically to ensure a canonical and deterministic representation
    nquads.sort();

    // Join sorted lines into bytes
    let canonical_bytes = nquads.join("").into_bytes();

    // Hash with BLAKE3
    let hash = blake3::hash(&canonical_bytes);

    Ok((canonical_bytes, hash))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{Construct8Packet, SymbolTable};
    use crate::stream::Construct8StreamExt;

    #[test]
    fn test_forge_canonical_determinism() {
        // We will create the same set of triples, but chunked and ordered differently.
        let symbols1 = SymbolTable::new();

        let s1 = symbols1.get_or_insert("<http://example.org/Alice>");
        let p1 = symbols1.get_or_insert("<http://example.org/knows>");
        let o1 = symbols1.get_or_insert("<http://example.org/Bob>");

        let s2 = symbols1.get_or_insert("<http://example.org/Bob>");
        let p2 = symbols1.get_or_insert("<http://example.org/knows>");
        let o2 = symbols1.get_or_insert("<http://example.org/Charlie>");

        let s3 = symbols1.get_or_insert("<http://example.org/Charlie>");
        let p3 = symbols1.get_or_insert("<http://example.org/knows>");
        let o3 = symbols1.get_or_insert("<http://example.org/Alice>");

        // Stream order: Triple 1, Triple 2, Triple 3
        let triples1 = vec![(s1, p1, o1), (s2, p2, o2), (s3, p3, o3)];
        let packets1: Vec<Construct8Packet> =
            triples1.into_iter().into_construct8_chunks().collect();

        let (bytes1, hash1) = forge_canonical(&packets1, &symbols1).unwrap();

        // Now create a different symbol table and insert in reverse/different order
        let symbols2 = SymbolTable::new();

        // Note: the IDs will be different here since we insert in reverse order!
        // Charlie knows Alice first
        let s3_b = symbols2.get_or_insert("<http://example.org/Charlie>");
        let p3_b = symbols2.get_or_insert("<http://example.org/knows>");
        let o3_b = symbols2.get_or_insert("<http://example.org/Alice>");

        // Bob knows Charlie next
        let s2_b = symbols2.get_or_insert("<http://example.org/Bob>");
        let p2_b = symbols2.get_or_insert("<http://example.org/knows>");
        let o2_b = symbols2.get_or_insert("<http://example.org/Charlie>");

        // Alice knows Bob last
        let s1_b = symbols2.get_or_insert("<http://example.org/Alice>");
        let p1_b = symbols2.get_or_insert("<http://example.org/knows>");
        let o1_b = symbols2.get_or_insert("<http://example.org/Bob>");

        // Stream order is different (reverse) but packaged in the same number of packets (1 packet)
        let triples2 = vec![(s3_b, p3_b, o3_b), (s2_b, p2_b, o2_b), (s1_b, p1_b, o1_b)];
        let packets2: Vec<Construct8Packet> =
            triples2.into_iter().into_construct8_chunks().collect();

        let (bytes2, hash2) = forge_canonical(&packets2, &symbols2).unwrap();

        // Verify they produce identical canonical byte streams
        assert_eq!(bytes1, bytes2);
        // Verify they compute identical BLAKE3 receipt hashes
        assert_eq!(hash1, hash2);
    }
}
