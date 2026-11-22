//! DTCG 2025.10-aligned `*.brand.toml` rhythm schema (ECOSYSTEM-SPEC-27.1.1 D4 Gate-1 slice).
//!
//! Parses human-authored visual-rhythm manifests and emits typed refusals for
//! incomplete manifests (missing touch floor / gap / grid), touch floors below
//! law, inter-target gaps below law, and off-grid spacing. Full brand token
//! inventories (color/type/CASTLE) remain Gate 2+.

use serde::Deserialize;

use super::taffy_aot::{MIN_GAP_PX, MIN_TOUCH_TARGET_PX};

/// Admitted DTCG revision for Gate-1 brand manifests.
pub const DTCG_VERSION: &str = "2025.10";

/// Material Design touch-target floor (CSS px).
pub const TOUCH_FLOOR_MATERIAL_PX: f32 = 48.0;

/// Apple HIG touch-target floor (CSS px) — same law as [`MIN_TOUCH_TARGET_PX`].
pub const TOUCH_FLOOR_APPLE_PX: f32 = MIN_TOUCH_TARGET_PX;

/// Minimum gap between interactive targets (CSS px).
pub const INTER_TARGET_GAP_PX: f32 = MIN_GAP_PX;

/// Primary spacing grid unit (CSS px).
pub const GRID_BASE_PX: u32 = 8;

/// Half-step on the spacing grid (CSS px) — 4px / 8px grid.
pub const GRID_HALF_PX: u32 = 4;

/// Parsed `*.brand.toml` document (rhythm plane only).
#[derive(Debug, Clone, PartialEq)]
pub struct BrandManifest {
    pub dtcg_version: String,
    pub brand_id: String,
    pub rhythm: BrandRhythm,
}

/// Visual rhythm block: rem/grid primitives, touch floors, spacing, fluid clamps.
#[derive(Debug, Clone, PartialEq)]
pub struct BrandRhythm {
    pub rem_base_px: f32,
    pub grid_base_px: u32,
    pub grid_half_px: u32,
    pub touch_floor_material_px: f32,
    pub touch_floor_apple_px: f32,
    pub inter_target_gap_px: f32,
    pub spacing: Vec<SpacingToken>,
    pub fluid: Vec<FluidToken>,
}

/// Discrete spacing token whose `px` must land on the 4px/8px grid.
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct SpacingToken {
    pub name: String,
    pub px: f32,
}

/// Fluid `clamp()` / `cqi` slope token.
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct FluidToken {
    pub name: String,
    pub min_px: f32,
    /// Preferred track — typically a `cqi` or `rem` expression (opaque string).
    pub preferred: String,
    pub max_px: f32,
}

/// Machine-readable admit receipt for a valid brand rhythm.
#[derive(Debug, Clone, PartialEq)]
pub struct BrandReceipt {
    pub brand_id: String,
    pub dtcg_version: String,
    pub spacing_count: usize,
    pub fluid_count: usize,
}

/// Gate-1 rhythm facets that must be present for completeness.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BrandCompletenessFacet {
    /// Material and/or Apple touch-floor keys.
    TouchFloor,
    /// Inter-target gap key.
    Gap,
    /// 4px/8px grid keys (`grid_base_px` / `grid_half_px`).
    Grid,
}

/// Typed refusals for brand rhythm law failures.
#[derive(Debug, Clone, PartialEq)]
pub enum BrandRefusal {
    Parse(String),
    /// Manifest omits a required Gate-1 rhythm facet (completeness court).
    Incomplete {
        missing: BrandCompletenessFacet,
        field: &'static str,
    },
    DtcgVersion {
        got: String,
        expected: String,
    },
    TouchFloorTooSmall {
        platform: &'static str,
        value_px: f32,
        min_px: u32,
    },
    GapTooSmall {
        gap_px: f32,
        min_px: u32,
    },
    OffGridSpacing {
        token: String,
        value_px: f32,
        grid_half_px: u32,
    },
    OffGridFluid {
        token: String,
        field: &'static str,
        value_px: f32,
        grid_half_px: u32,
    },
    GridMismatch {
        field: &'static str,
        got: u32,
        expected: u32,
    },
    RemBaseInvalid {
        rem_base_px: f32,
    },
}

/// Wire shape: optional Gate-1 facets so missing keys become typed incompleteness.
#[derive(Debug, Deserialize)]
struct BrandManifestWire {
    dtcg_version: String,
    brand_id: String,
    rhythm: BrandRhythmWire,
}

#[derive(Debug, Deserialize)]
struct BrandRhythmWire {
    rem_base_px: Option<f32>,
    grid_base_px: Option<u32>,
    grid_half_px: Option<u32>,
    touch_floor_material_px: Option<f32>,
    touch_floor_apple_px: Option<f32>,
    inter_target_gap_px: Option<f32>,
    #[serde(default)]
    spacing: Vec<SpacingToken>,
    #[serde(default)]
    fluid: Vec<FluidToken>,
}

/// Parse a `*.brand.toml` document from UTF-8 text.
///
/// Missing touch-floor, gap, or grid keys refuse as [`BrandRefusal::Incomplete`]
/// rather than an opaque serde parse error.
pub fn parse_brand_toml(raw: &str) -> Result<BrandManifest, BrandRefusal> {
    let wire: BrandManifestWire =
        toml::from_str(raw).map_err(|e| BrandRefusal::Parse(e.to_string()))?;
    let rhythm = complete_rhythm(wire.rhythm)?;
    Ok(BrandManifest {
        dtcg_version: wire.dtcg_version,
        brand_id: wire.brand_id,
        rhythm,
    })
}

fn complete_rhythm(wire: BrandRhythmWire) -> Result<BrandRhythm, BrandRefusal> {
    let touch_floor_material_px = wire
        .touch_floor_material_px
        .ok_or(BrandRefusal::Incomplete {
            missing: BrandCompletenessFacet::TouchFloor,
            field: "touch_floor_material_px",
        })?;
    let touch_floor_apple_px = wire.touch_floor_apple_px.ok_or(BrandRefusal::Incomplete {
        missing: BrandCompletenessFacet::TouchFloor,
        field: "touch_floor_apple_px",
    })?;
    let inter_target_gap_px = wire.inter_target_gap_px.ok_or(BrandRefusal::Incomplete {
        missing: BrandCompletenessFacet::Gap,
        field: "inter_target_gap_px",
    })?;
    let grid_base_px = wire.grid_base_px.ok_or(BrandRefusal::Incomplete {
        missing: BrandCompletenessFacet::Grid,
        field: "grid_base_px",
    })?;
    let grid_half_px = wire.grid_half_px.ok_or(BrandRefusal::Incomplete {
        missing: BrandCompletenessFacet::Grid,
        field: "grid_half_px",
    })?;
    let rem_base_px = wire
        .rem_base_px
        .ok_or(BrandRefusal::RemBaseInvalid { rem_base_px: 0.0 })?;

    Ok(BrandRhythm {
        rem_base_px,
        grid_base_px,
        grid_half_px,
        touch_floor_material_px,
        touch_floor_apple_px,
        inter_target_gap_px,
        spacing: wire.spacing,
        fluid: wire.fluid,
    })
}

/// Admit a parsed brand rhythm under Gate-1 visual-rhythm law.
pub fn admit_brand(manifest: &BrandManifest) -> Result<BrandReceipt, BrandRefusal> {
    if manifest.dtcg_version != DTCG_VERSION {
        return Err(BrandRefusal::DtcgVersion {
            got: manifest.dtcg_version.clone(),
            expected: DTCG_VERSION.to_string(),
        });
    }

    let rhythm = &manifest.rhythm;

    if !(rhythm.rem_base_px > 0.0) {
        return Err(BrandRefusal::RemBaseInvalid {
            rem_base_px: rhythm.rem_base_px,
        });
    }

    if rhythm.grid_base_px != GRID_BASE_PX {
        return Err(BrandRefusal::GridMismatch {
            field: "grid_base_px",
            got: rhythm.grid_base_px,
            expected: GRID_BASE_PX,
        });
    }

    if rhythm.grid_half_px != GRID_HALF_PX {
        return Err(BrandRefusal::GridMismatch {
            field: "grid_half_px",
            got: rhythm.grid_half_px,
            expected: GRID_HALF_PX,
        });
    }

    if rhythm.touch_floor_apple_px + f32::EPSILON < TOUCH_FLOOR_APPLE_PX {
        return Err(BrandRefusal::TouchFloorTooSmall {
            platform: "apple",
            value_px: rhythm.touch_floor_apple_px,
            min_px: TOUCH_FLOOR_APPLE_PX as u32,
        });
    }

    if rhythm.touch_floor_material_px + f32::EPSILON < TOUCH_FLOOR_MATERIAL_PX {
        return Err(BrandRefusal::TouchFloorTooSmall {
            platform: "material",
            value_px: rhythm.touch_floor_material_px,
            min_px: TOUCH_FLOOR_MATERIAL_PX as u32,
        });
    }

    if rhythm.inter_target_gap_px + f32::EPSILON < INTER_TARGET_GAP_PX {
        return Err(BrandRefusal::GapTooSmall {
            gap_px: rhythm.inter_target_gap_px,
            min_px: INTER_TARGET_GAP_PX as u32,
        });
    }

    for token in &rhythm.spacing {
        if !is_on_grid(token.px, rhythm.grid_half_px) {
            return Err(BrandRefusal::OffGridSpacing {
                token: token.name.clone(),
                value_px: token.px,
                grid_half_px: rhythm.grid_half_px,
            });
        }
    }

    for fluid in &rhythm.fluid {
        if !is_on_grid(fluid.min_px, rhythm.grid_half_px) {
            return Err(BrandRefusal::OffGridFluid {
                token: fluid.name.clone(),
                field: "min_px",
                value_px: fluid.min_px,
                grid_half_px: rhythm.grid_half_px,
            });
        }
        if !is_on_grid(fluid.max_px, rhythm.grid_half_px) {
            return Err(BrandRefusal::OffGridFluid {
                token: fluid.name.clone(),
                field: "max_px",
                value_px: fluid.max_px,
                grid_half_px: rhythm.grid_half_px,
            });
        }
    }

    Ok(BrandReceipt {
        brand_id: manifest.brand_id.clone(),
        dtcg_version: manifest.dtcg_version.clone(),
        spacing_count: rhythm.spacing.len(),
        fluid_count: rhythm.fluid.len(),
    })
}

/// Parse then admit in one step.
pub fn parse_and_admit_brand(raw: &str) -> Result<BrandReceipt, BrandRefusal> {
    let manifest = parse_brand_toml(raw)?;
    admit_brand(&manifest)
}

fn is_on_grid(value_px: f32, grid_half_px: u32) -> bool {
    if grid_half_px == 0 {
        return false;
    }
    if !value_px.is_finite() || value_px < 0.0 {
        return false;
    }
    let half = grid_half_px as f32;
    let steps = (value_px / half).round();
    (value_px - steps * half).abs() <= f32::EPSILON * 8.0_f32.max(value_px.abs())
}
