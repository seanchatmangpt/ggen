use lsp_max::lsp_types::Position;

/// Robust mapper for converting between UTF-8 byte offsets and LSP UTF-16 positions.
///
/// LSP positions (line, character) use 0-based indexing and UTF-16 code units for
/// the character offset. Rust strings use UTF-8 byte offsets. This mapper
/// pre-calculates line starts and provides efficient, correct conversion handling
/// multi-byte characters and various line endings (LF, CRLF, CR).
pub struct LineColMapper {
    line_starts: Vec<usize>,
    text: String,
}

impl LineColMapper {
    /// Create a new mapper for the given text.
    #[must_use]
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        let bytes = text.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'\n' {
                line_starts.push(i + 1);
            } else if bytes[i] == b'\r' {
                if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                    i += 1;
                }
                line_starts.push(i + 1);
            }
            i += 1;
        }
        Self {
            line_starts,
            text: text.to_string(),
        }
    }

    /// Convert an LSP position (UTF-16) to a UTF-8 byte offset.
    ///
    /// Returns `None` if the line index is out of bounds. If the character offset
    /// exceeds the line length, it defaults to the end of the line (per LSP spec).
    #[must_use]
    pub fn position_to_byte(&self, position: Position) -> Option<usize> {
        let line_idx = position.line as usize;
        let line_start = *self.line_starts.get(line_idx)?;

        // Get text from line start to end of file
        let remaining_text = self.text.get(line_start..)?;

        let mut current_utf16_at = 0;
        let target_character = position.character as usize;

        for (offset, c) in remaining_text.char_indices() {
            if current_utf16_at == target_character {
                return Some(line_start + offset);
            }

            // If we hit a newline, we've reached the end of the requested line
            if c == '\n' || c == '\r' {
                break;
            }

            current_utf16_at += c.len_utf16();
        }

        // If we finished the loop and current_utf16_at <= target_character,
        // it means target_character is at or beyond the end of the line.
        // LSP spec says to default to line length.
        let line_end_offset = remaining_text
            .find(|c| c == '\n' || c == '\r')
            .unwrap_or(remaining_text.len());

        Some(line_start + line_end_offset)
    }

    /// Convert a UTF-8 byte offset to an LSP position (UTF-16).
    ///
    /// Returns `None` if the byte offset is out of bounds or points into the
    /// middle of a multi-byte character.
    #[must_use]
    pub fn byte_to_position(&self, byte_offset: usize) -> Option<Position> {
        if byte_offset > self.text.len() {
            return None;
        }

        // Find the line containing the byte offset
        let line = match self.line_starts.binary_search(&byte_offset) {
            Ok(line) => line,
            Err(line) => line - 1,
        };

        let line_start = self.line_starts[line];
        // Ensure we don't slice in the middle of a multi-byte character
        let line_text = self.text.get(line_start..byte_offset)?;

        let mut character = 0;
        for c in line_text.chars() {
            character += c.len_utf16();
        }

        Some(Position {
            line: line as u32,
            character: character as u32,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_col_mapper_basic() {
        let text = "hello\nworld";
        let mapper = LineColMapper::new(text);

        assert_eq!(
            mapper.byte_to_position(0),
            Some(Position {
                line: 0,
                character: 0
            })
        );
        assert_eq!(
            mapper.byte_to_position(5),
            Some(Position {
                line: 0,
                character: 5
            })
        );
        assert_eq!(
            mapper.byte_to_position(6),
            Some(Position {
                line: 1,
                character: 0
            })
        );
        assert_eq!(
            mapper.byte_to_position(11),
            Some(Position {
                line: 1,
                character: 5
            })
        );

        assert_eq!(
            mapper.position_to_byte(Position {
                line: 0,
                character: 0
            }),
            Some(0)
        );
        assert_eq!(
            mapper.position_to_byte(Position {
                line: 0,
                character: 5
            }),
            Some(5)
        );
        assert_eq!(
            mapper.position_to_byte(Position {
                line: 1,
                character: 0
            }),
            Some(6)
        );
        assert_eq!(
            mapper.position_to_byte(Position {
                line: 1,
                character: 5
            }),
            Some(11)
        );
    }

    #[test]
    fn test_line_col_mapper_utf16() {
        // "𝄞" (U+1D11E) is 4 bytes in UTF-8 and 2 code units in UTF-16
        let text = "𝄞 music";
        let mapper = LineColMapper::new(text);

        // Byte 0 is start of 𝄞
        assert_eq!(
            mapper.byte_to_position(0),
            Some(Position {
                line: 0,
                character: 0
            })
        );
        // Byte 4 is after 𝄞 (space)
        assert_eq!(
            mapper.byte_to_position(4),
            Some(Position {
                line: 0,
                character: 2
            })
        );

        assert_eq!(
            mapper.position_to_byte(Position {
                line: 0,
                character: 0
            }),
            Some(0)
        );
        assert_eq!(
            mapper.position_to_byte(Position {
                line: 0,
                character: 2
            }),
            Some(4)
        );
    }

    #[test]
    fn test_line_col_mapper_line_endings() {
        let text = "a\rb\r\nc";
        let mapper = LineColMapper::new(text);

        assert_eq!(mapper.line_starts, vec![0, 2, 5]);
        assert_eq!(
            mapper.byte_to_position(0),
            Some(Position {
                line: 0,
                character: 0
            })
        ); // 'a'
        assert_eq!(
            mapper.byte_to_position(2),
            Some(Position {
                line: 1,
                character: 0
            })
        ); // 'b'
        assert_eq!(
            mapper.byte_to_position(5),
            Some(Position {
                line: 2,
                character: 0
            })
        ); // 'c'
    }

    #[test]
    fn test_line_col_mapper_overflow() {
        let text = "abc";
        let mapper = LineColMapper::new(text);

        // Character 10 on line 0 (which is only 3 chars long)
        assert_eq!(
            mapper.position_to_byte(Position {
                line: 0,
                character: 10
            }),
            Some(3)
        );

        // Line 1 (does not exist)
        assert_eq!(
            mapper.position_to_byte(Position {
                line: 1,
                character: 0
            }),
            None
        );
    }
}
