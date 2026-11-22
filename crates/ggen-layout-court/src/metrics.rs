//! Product-subject Taffy metrics court (stay-in-bounds + pairwise non-overlap).

use serde::Deserialize;
use taffy::prelude::*;
use taffy::TaffyTree;

use crate::text_spacing::{spacing_required_height, spacing_width_delta, TextMetrics1412};

/// AOT viewport sweep mirrored by zcode-cli `UI_LAYOUT_AOT_VIEWPORTS`.
pub const VIEWPORTS: &[f32] = &[320.0, 375.0, 393.0, 600.0, 840.0, 1280.0];

pub const MIN_TOUCH_TARGET_PX: f32 = 44.0;
pub const MIN_TOUCH_GAP_PX: f32 = 8.0;

#[derive(Debug, Clone, Deserialize)]
pub struct ProductSubject {
    pub root: String,
    #[serde(default)]
    pub hard_fail_undersize: bool,
    pub nodes: Vec<ProductNode>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ProductNode {
    pub id: String,
    #[serde(default)]
    pub interactive: bool,
    #[serde(default)]
    pub primary: bool,
    #[serde(default)]
    pub style: NodeStyle,
    #[serde(default)]
    pub children: Vec<String>,
    #[serde(default)]
    pub text: Option<NodeText>,
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct NodeStyle {
    #[serde(default)]
    pub display: Option<String>,
    #[serde(default)]
    pub flex_direction: Option<String>,
    #[serde(default)]
    pub gap: Option<GapSpec>,
    #[serde(default)]
    pub size: Option<SizeSpec>,
    #[serde(default)]
    pub flex_grow: Option<f32>,
    #[serde(default)]
    pub flex_shrink: Option<f32>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct GapSpec {
    #[serde(default)]
    pub column: f32,
    #[serde(default)]
    pub row: f32,
}

#[derive(Debug, Clone, Deserialize)]
pub struct SizeSpec {
    #[serde(default)]
    pub width: Option<DimSpec>,
    #[serde(default)]
    pub height: Option<DimSpec>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum DimSpec {
    Px { px: f32 },
    Percent { percent: f32 },
}

#[derive(Debug, Clone, Deserialize)]
pub struct NodeText {
    pub font_size: f32,
    #[serde(default = "one")]
    pub line_count: u32,
    #[serde(default = "one")]
    pub paragraph_count: u32,
    #[serde(default)]
    pub char_count: u32,
    #[serde(default)]
    pub word_count: u32,
}

fn one() -> u32 {
    1
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProductRefusal {
    Parse {
        message: String,
    },
    MissingRoot {
        root: String,
    },
    LayoutEngine {
        message: String,
    },
    Overflow {
        viewport_width_px: u32,
        leaf_id: String,
        content_right_px: f32,
    },
    Overlap {
        viewport_width_px: u32,
        a_id: String,
        b_id: String,
    },
    TouchTargetTooSmall {
        viewport_width_px: u32,
        leaf_id: String,
        width_px: f32,
        height_px: f32,
    },
    TouchGapTooSmall {
        viewport_width_px: u32,
        a_id: String,
        b_id: String,
        gap_px: f32,
    },
    TextSpacingOverflow {
        viewport_width_px: u32,
        leaf_id: String,
        content_right_px: f32,
    },
}

impl ProductRefusal {
    pub fn code(&self) -> &'static str {
        match self {
            Self::Parse { .. } => "REFUSED_LAYOUT_PARSE",
            Self::MissingRoot { .. } => "REFUSED_LAYOUT_MISSING_ROOT",
            Self::LayoutEngine { .. } => "REFUSED_LAYOUT_ENGINE",
            Self::Overflow { .. } => "REFUSED_LAYOUT_OVERFLOW",
            Self::Overlap { .. } => "REFUSED_LAYOUT_OVERLAP",
            Self::TouchTargetTooSmall { .. } => "REFUSED_LAYOUT_TOUCH",
            Self::TouchGapTooSmall { .. } => "REFUSED_LAYOUT_TOUCH_GAP",
            Self::TextSpacingOverflow { .. } => "REFUSED_LAYOUT_TEXT_SPACING",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProductReceipt {
    pub root: String,
    pub viewports_px: Vec<u32>,
    pub node_count: usize,
}

fn dim_to_taffy(d: &Option<DimSpec>) -> Dimension {
    match d {
        Some(DimSpec::Px { px }) => Dimension::length(*px),
        Some(DimSpec::Percent { percent }) => Dimension::percent(*percent),
        None => Dimension::auto(),
    }
}

fn style_for(node: &ProductNode) -> Style {
    let display = match node.style.display.as_deref() {
        Some("none") => Display::None,
        Some("grid") => Display::Grid,
        _ => Display::Flex,
    };
    let flex_direction = match node.style.flex_direction.as_deref() {
        Some("row") => FlexDirection::Row,
        Some("row-reverse") => FlexDirection::RowReverse,
        Some("column-reverse") => FlexDirection::ColumnReverse,
        _ => FlexDirection::Column,
    };
    let (gap_col, gap_row) = match &node.style.gap {
        Some(g) => (g.column, g.row),
        None => (0.0, 0.0),
    };
    let size = match &node.style.size {
        Some(s) => Size {
            width: dim_to_taffy(&s.width),
            height: dim_to_taffy(&s.height),
        },
        None => Size::auto(),
    };
    Style {
        display,
        flex_direction,
        size,
        gap: Size {
            width: LengthPercentage::length(gap_col),
            height: LengthPercentage::length(gap_row),
        },
        flex_grow: node.style.flex_grow.unwrap_or(0.0),
        flex_shrink: node.style.flex_shrink.unwrap_or(1.0),
        ..Default::default()
    }
}

pub fn run_product_subject(
    subject: &ProductSubject, viewports: &[f32],
) -> Result<ProductReceipt, ProductRefusal> {
    if subject.nodes.is_empty() {
        return Err(ProductRefusal::Parse {
            message: "product-subject nodes[] is empty".into(),
        });
    }
    if !subject.nodes.iter().any(|n| n.id == subject.root) {
        return Err(ProductRefusal::MissingRoot {
            root: subject.root.clone(),
        });
    }

    let mut widths_px = Vec::with_capacity(viewports.len());
    for &vw in viewports {
        court_one_viewport(subject, vw)?;
        widths_px.push(vw.round() as u32);
    }

    Ok(ProductReceipt {
        root: subject.root.clone(),
        viewports_px: widths_px,
        node_count: subject.nodes.len(),
    })
}

fn court_one_viewport(
    subject: &ProductSubject, viewport_width_px: f32,
) -> Result<(), ProductRefusal> {
    let viewport_u = viewport_width_px.round() as u32;
    let mut tree: TaffyTree<()> = TaffyTree::new();
    let mut id_to_node: std::collections::BTreeMap<String, NodeId> =
        std::collections::BTreeMap::new();

    for node in &subject.nodes {
        let nid = tree
            .new_leaf(style_for(node))
            .map_err(|e| ProductRefusal::LayoutEngine {
                message: format!("new_leaf({}): {e}", node.id),
            })?;
        id_to_node.insert(node.id.clone(), nid);
    }

    for node in &subject.nodes {
        if node.children.is_empty() {
            continue;
        }
        let parent = *id_to_node
            .get(&node.id)
            .ok_or_else(|| ProductRefusal::LayoutEngine {
                message: format!("missing parent {}", node.id),
            })?;
        let mut kids = Vec::with_capacity(node.children.len());
        for cid in &node.children {
            let kid = *id_to_node
                .get(cid)
                .ok_or_else(|| ProductRefusal::LayoutEngine {
                    message: format!("missing child {cid} of {}", node.id),
                })?;
            kids.push(kid);
        }
        tree.set_children(parent, &kids)
            .map_err(|e| ProductRefusal::LayoutEngine {
                message: format!("set_children({}): {e}", node.id),
            })?;
        tree.set_style(parent, style_for(node))
            .map_err(|e| ProductRefusal::LayoutEngine {
                message: format!("set_style({}): {e}", node.id),
            })?;
    }

    let root = *id_to_node
        .get(&subject.root)
        .ok_or_else(|| ProductRefusal::MissingRoot {
            root: subject.root.clone(),
        })?;

    tree.compute_layout(
        root,
        Size {
            width: AvailableSpace::Definite(viewport_width_px),
            height: AvailableSpace::MaxContent,
        },
    )
    .map_err(|e| ProductRefusal::LayoutEngine {
        message: format!("compute_layout: {e}"),
    })?;

    let mut rects: Vec<(String, f32, f32, f32, f32, bool, Option<TextMetrics1412>)> =
        Vec::with_capacity(subject.nodes.len());

    for node in &subject.nodes {
        let nid = id_to_node[&node.id];
        let layout = tree.layout(nid).map_err(|e| ProductRefusal::LayoutEngine {
            message: format!("layout({}): {e}", node.id),
        })?;
        let left = layout.location.x;
        let top = layout.location.y;
        let width = layout.size.width;
        let height = layout.size.height;
        let right = left + width;
        let bottom = top + height;

        if right > viewport_width_px + 0.5 {
            return Err(ProductRefusal::Overflow {
                viewport_width_px: viewport_u,
                leaf_id: node.id.clone(),
                content_right_px: right,
            });
        }

        if node.interactive
            && (width + f32::EPSILON < MIN_TOUCH_TARGET_PX
                || height + f32::EPSILON < MIN_TOUCH_TARGET_PX)
        {
            return Err(ProductRefusal::TouchTargetTooSmall {
                viewport_width_px: viewport_u,
                leaf_id: node.id.clone(),
                width_px: width,
                height_px: height,
            });
        }

        let text = node.text.as_ref().map(|t| TextMetrics1412 {
            font_size: t.font_size,
            line_count: t.line_count,
            paragraph_count: t.paragraph_count,
            char_count: t.char_count,
            word_count: t.word_count,
        });

        rects.push((
            node.id.clone(),
            left,
            top,
            right,
            bottom,
            node.interactive,
            text,
        ));
    }

    let leaf_rects: Vec<_> = rects
        .iter()
        .filter(|(id, ..)| {
            subject
                .nodes
                .iter()
                .find(|n| n.id == *id)
                .map(|n| n.children.is_empty())
                .unwrap_or(true)
        })
        .cloned()
        .collect();

    for i in 0..leaf_rects.len() {
        for j in (i + 1)..leaf_rects.len() {
            let (ref a_id, a_l, a_t, a_r, a_b, a_ix, _) = leaf_rects[i];
            let (ref b_id, b_l, b_t, b_r, b_b, b_ix, _) = leaf_rects[j];
            let overlap_x = a_l < b_r && b_l < a_r;
            let overlap_y = a_t < b_b && b_t < a_b;
            if overlap_x && overlap_y {
                return Err(ProductRefusal::Overlap {
                    viewport_width_px: viewport_u,
                    a_id: a_id.clone(),
                    b_id: b_id.clone(),
                });
            }
            if a_ix && b_ix {
                let gap_x = if a_r <= b_l {
                    b_l - a_r
                } else if b_r <= a_l {
                    a_l - b_r
                } else {
                    0.0
                };
                let gap_y = if a_b <= b_t {
                    b_t - a_b
                } else if b_b <= a_t {
                    a_t - b_b
                } else {
                    0.0
                };
                let gap = if gap_x > 0.0 && gap_y > 0.0 {
                    (gap_x * gap_x + gap_y * gap_y).sqrt()
                } else if gap_x > 0.0 {
                    gap_x
                } else {
                    gap_y
                };
                if gap + f32::EPSILON < MIN_TOUCH_GAP_PX {
                    return Err(ProductRefusal::TouchGapTooSmall {
                        viewport_width_px: viewport_u,
                        a_id: a_id.clone(),
                        b_id: b_id.clone(),
                        gap_px: gap,
                    });
                }
            }
        }
    }

    for (id, left, _top, right, _bottom, _ix, text) in &leaf_rects {
        let Some(m) = text else { continue };
        let expanded_right = left + (right - left) + spacing_width_delta(m);
        let _ = spacing_required_height(m);
        if expanded_right > viewport_width_px + 0.5 {
            return Err(ProductRefusal::TextSpacingOverflow {
                viewport_width_px: viewport_u,
                leaf_id: id.clone(),
                content_right_px: expanded_right,
            });
        }
    }

    let _ = subject.hard_fail_undersize;
    Ok(())
}

pub fn parse_product_subject(json: &str) -> Result<ProductSubject, ProductRefusal> {
    serde_json::from_str(json).map_err(|e| ProductRefusal::Parse {
        message: e.to_string(),
    })
}
