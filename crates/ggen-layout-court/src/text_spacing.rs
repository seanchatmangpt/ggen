//! WCAG 1.4.12 text-spacing constants (mirrored by zcode-cli ui-layout-court).

pub const WCAG_1412_LINE_HEIGHT_MULT: f32 = 1.5;
pub const WCAG_1412_PARAGRAPH_SPACING_MULT: f32 = 2.0;
pub const WCAG_1412_LETTER_SPACING_EM: f32 = 0.12;
pub const WCAG_1412_WORD_SPACING_EM: f32 = 0.16;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TextMetrics1412 {
    pub font_size: f32,
    pub line_count: u32,
    pub paragraph_count: u32,
    pub char_count: u32,
    pub word_count: u32,
}

pub fn spacing_width_delta(m: &TextMetrics1412) -> f32 {
    let letter_gaps = m.char_count.saturating_sub(1) as f32;
    let word_gaps = m.word_count.saturating_sub(1) as f32;
    letter_gaps * m.font_size * WCAG_1412_LETTER_SPACING_EM
        + word_gaps * m.font_size * WCAG_1412_WORD_SPACING_EM
}

pub fn spacing_required_height(m: &TextMetrics1412) -> f32 {
    let lines = m.line_count.max(1) as f32;
    let para_gaps = m.paragraph_count.saturating_sub(1) as f32;
    lines * m.font_size * WCAG_1412_LINE_HEIGHT_MULT
        + para_gaps * m.font_size * WCAG_1412_PARAGRAPH_SPACING_MULT
}
