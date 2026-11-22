//! Taffy AOT layout pre-flight (ECOSYSTEM-SPEC-27.1.1 Gate 1 D2).
//!
//! Evaluates a column of leaves at a viewport width under a WCAG 1.4.12 text-spacing
//! multiplier. Emits typed refusals for overflow, overlap, touch targets &lt; 44px, and
//! gaps &lt; 8px. The six-width seal ([`preflight_column_six_widths`]) admits only when
//! every Gate-1 viewport passes under the supplied spacing factor.

use serde::{Deserialize, Serialize};
use taffy::prelude::*;
use taffy::TaffyTree;

/// Viewport widths required by ECOSYSTEM-SPEC-27.1.1 §6 (CSS px).
pub const VIEWPORT_WIDTHS_PX: &[f32] = &[320.0, 375.0, 393.0, 600.0, 840.0, 1280.0];

/// WCAG 1.4.12 text-spacing re-evaluation factor used by Gate-1 courts.
pub const WCAG_1412_TEXT_SPACING: f32 = 1.5;

/// Minimum interactive touch target (CSS px) enforced by this stub.
pub const MIN_TOUCH_TARGET_PX: f32 = 44.0;

/// Minimum gap between siblings (CSS px) enforced by this stub.
pub const MIN_GAP_PX: f32 = 8.0;

/// A leaf box in an AOT column fixture.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AotLeaf {
    pub id: String,
    pub width_px: f32,
    pub height_px: f32,
    /// When true, width and height scale by `text_spacing` (WCAG 1.4.12 re-eval).
    pub is_text: bool,
    /// When true, both axes must be ≥ [`MIN_TOUCH_TARGET_PX`].
    pub is_interactive: bool,
}

/// Flex column of leaves with a uniform sibling gap.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AotColumn {
    pub gap_px: f32,
    pub children: Vec<AotLeaf>,
}

/// Machine-readable admit receipt for a successful single-width pre-flight.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AotReceipt {
    pub viewport_width_px: u32,
    pub text_spacing: f32,
    pub leaf_count: usize,
}

/// Machine-readable admit receipt for a successful six-width seal.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AotSealReceipt {
    pub text_spacing: f32,
    /// Exact Gate-1 widths that admitted, in seal order.
    pub widths_px: Vec<u32>,
    pub leaf_count: usize,
}

/// Typed refusals for AOT geometry law failures.
///
/// Public surface name: [`LayoutRefusal`]. Every geometry variant carries
/// `viewport_width_px` so courts fail closed naming the failing width.
#[derive(Debug, Clone, PartialEq)]
pub enum AotRefusal {
    Overflow {
        viewport_width_px: u32,
        text_spacing: f32,
        leaf_id: String,
        content_right_px: f32,
    },
    Overlap {
        viewport_width_px: u32,
        text_spacing: f32,
        a_id: String,
        b_id: String,
    },
    TouchTargetTooSmall {
        viewport_width_px: u32,
        text_spacing: f32,
        leaf_id: String,
        width_px: f32,
        height_px: f32,
        min_px: u32,
    },
    GapTooSmall {
        viewport_width_px: u32,
        text_spacing: f32,
        gap_px: f32,
        min_px: u32,
    },
    /// Taffy tree construction / layout failed (fail-closed).
    LayoutEngine { message: String },
}

/// Gate-1 layout typed-error surface (alias of [`AotRefusal`]).
pub type LayoutRefusal = AotRefusal;

/// Run Taffy AOT pre-flight for `column` at `viewport_width_px` under `text_spacing`.
///
/// Text leaves have their preferred size multiplied by `text_spacing` before layout
/// (narrow model of WCAG 1.4.12 1.5× re-evaluation). Interactive leaves are checked
/// against the 44px touch-target floor on the *laid-out* size; non-negative column
/// `gap_px` must be ≥ 8px whenever two or more children are present. Negative gaps
/// skip the gap floor so overlap courts can exercise [`AotRefusal::Overlap`].
pub fn preflight_column(
    column: &AotColumn, viewport_width_px: f32, text_spacing: f32,
) -> Result<AotReceipt, AotRefusal> {
    let viewport_u = viewport_width_px.round() as u32;

    // Non-negative gaps below the floor refuse as GapTooSmall. Negative gaps are
    // an adversarial fixture path: they skip this gate and may yield Overlap after
    // layout (fail-closed with width + node ids).
    if column.children.len() >= 2
        && column.gap_px >= 0.0
        && column.gap_px + f32::EPSILON < MIN_GAP_PX
    {
        return Err(AotRefusal::GapTooSmall {
            viewport_width_px: viewport_u,
            text_spacing,
            gap_px: column.gap_px,
            min_px: MIN_GAP_PX as u32,
        });
    }

    let mut tree: TaffyTree<()> = TaffyTree::new();
    let mut child_ids = Vec::with_capacity(column.children.len());
    let mut meta: Vec<(&AotLeaf, f32, f32)> = Vec::with_capacity(column.children.len());

    for leaf in &column.children {
        let (w, h) = if leaf.is_text {
            (leaf.width_px * text_spacing, leaf.height_px * text_spacing)
        } else {
            (leaf.width_px, leaf.height_px)
        };
        meta.push((leaf, w, h));

        let node = tree
            .new_leaf(Style {
                size: Size {
                    width: Dimension::length(w),
                    height: Dimension::length(h),
                },
                flex_shrink: 0.0,
                ..Default::default()
            })
            .map_err(|e| AotRefusal::LayoutEngine {
                message: format!("new_leaf({}): {e}", leaf.id),
            })?;
        child_ids.push(node);
    }

    let root = tree
        .new_with_children(
            Style {
                display: Display::Flex,
                flex_direction: FlexDirection::Column,
                size: Size {
                    width: Dimension::length(viewport_width_px),
                    height: Dimension::auto(),
                },
                gap: Size {
                    width: LengthPercentage::length(0.0),
                    // Preserve signed gap so negative fixtures can surface Overlap.
                    height: LengthPercentage::length(column.gap_px),
                },
                ..Default::default()
            },
            &child_ids,
        )
        .map_err(|e| AotRefusal::LayoutEngine {
            message: format!("new_with_children: {e}"),
        })?;

    tree.compute_layout(
        root,
        Size {
            width: AvailableSpace::Definite(viewport_width_px),
            height: AvailableSpace::MaxContent,
        },
    )
    .map_err(|e| AotRefusal::LayoutEngine {
        message: format!("compute_layout: {e}"),
    })?;

    let mut rects: Vec<(String, f32, f32, f32, f32)> = Vec::with_capacity(child_ids.len());

    for (node, (leaf, _pref_w, _pref_h)) in child_ids.iter().zip(meta.iter()) {
        let layout = tree.layout(*node).map_err(|e| AotRefusal::LayoutEngine {
            message: format!("layout({}): {e}", leaf.id),
        })?;
        let left = layout.location.x;
        let top = layout.location.y;
        let right = left + layout.size.width;
        let bottom = top + layout.size.height;

        if right > viewport_width_px + 0.5 {
            return Err(AotRefusal::Overflow {
                viewport_width_px: viewport_u,
                text_spacing,
                leaf_id: leaf.id.clone(),
                content_right_px: right,
            });
        }

        if leaf.is_interactive
            && (layout.size.width + f32::EPSILON < MIN_TOUCH_TARGET_PX
                || layout.size.height + f32::EPSILON < MIN_TOUCH_TARGET_PX)
        {
            return Err(AotRefusal::TouchTargetTooSmall {
                viewport_width_px: viewport_u,
                text_spacing,
                leaf_id: leaf.id.clone(),
                width_px: layout.size.width,
                height_px: layout.size.height,
                min_px: MIN_TOUCH_TARGET_PX as u32,
            });
        }

        rects.push((leaf.id.clone(), left, top, right, bottom));
    }

    for i in 0..rects.len() {
        for j in (i + 1)..rects.len() {
            let (ref a_id, a_l, a_t, a_r, a_b) = rects[i];
            let (ref b_id, b_l, b_t, b_r, b_b) = rects[j];
            let overlap_x = a_l < b_r && b_l < a_r;
            let overlap_y = a_t < b_b && b_t < a_b;
            if overlap_x && overlap_y {
                return Err(AotRefusal::Overlap {
                    viewport_width_px: viewport_u,
                    text_spacing,
                    a_id: a_id.clone(),
                    b_id: b_id.clone(),
                });
            }
        }
    }

    Ok(AotReceipt {
        viewport_width_px: viewport_u,
        text_spacing,
        leaf_count: column.children.len(),
    })
}

/// Seal `column` across every Gate-1 viewport in [`VIEWPORT_WIDTHS_PX`] under `text_spacing`.
///
/// Fail-closed: the first width that refuses wins. Typed refusals already carry
/// `viewport_width_px`, so a layout that only breaks at one width (e.g. 320) is
/// refused naming that width.
pub fn preflight_column_six_widths(
    column: &AotColumn, text_spacing: f32,
) -> Result<AotSealReceipt, AotRefusal> {
    let mut widths_px = Vec::with_capacity(VIEWPORT_WIDTHS_PX.len());
    for &width in VIEWPORT_WIDTHS_PX {
        let receipt = preflight_column(column, width, text_spacing)?;
        widths_px.push(receipt.viewport_width_px);
    }
    Ok(AotSealReceipt {
        text_spacing,
        widths_px,
        leaf_count: column.children.len(),
    })
}
