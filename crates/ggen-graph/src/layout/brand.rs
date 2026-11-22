//! DTCG 2025.10-aligned `*.brand.toml` rhythm schema (ECOSYSTEM-SPEC-27.1.1 D4 Gate-1 slice).
//!
//! Parses human-authored visual-rhythm manifests and emits typed refusals for
//! touch floors, inter-target gaps, off-grid spacing, and fluid `clamp()` /
//! `cqi` slope law. Optional brand / patronage messaging slots are accepted as
//! plain TOML strings (no private ontology namespaces). Full brand token
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

/// Optional `[brand]` identity block (castle-publish twin keys).
#[derive(Debug, Clone, PartialEq, Deserialize, Default)]
pub struct BrandIdentity {
    pub id: Option<String>,
    pub name: Option<String>,
    pub short_name: Option<String>,
    pub tagline: Option<String>,
    /// House standing copy (e.g. `"woman-owned house"`).
    pub house: Option<String>,
}

/// CindyCat-first patronage copy slots — plain strings only.
#[derive(Debug, Clone, PartialEq, Deserialize, Default)]
pub struct BrandPatronage {
    pub headline: Option<String>,
    pub fair_pay: Option<String>,
    pub custody: Option<String>,
    pub skimming: Option<String>,
    pub tagline: Option<String>,
    pub house: Option<String>,
}

/// Optional commercial floors mirrored from castle brand twins.
#[derive(Debug, Clone, PartialEq, Deserialize, Default)]
pub struct BrandCommercial {
    pub custody_pct: Option<f64>,
    pub skimming_pct: Option<f64>,
    pub transaction_rent_pct: Option<f64>,
}

/// Optional messaging tables; absent on Fourth Watch Gate-1 rhythm stub.
#[derive(Debug, Clone, PartialEq, Deserialize, Default)]
pub struct BrandMessaging {
    #[serde(default)]
    pub patronage: BrandPatronage,
    #[serde(default)]
    pub commercial: BrandCommercial,
}

/// Parsed `*.brand.toml` document (rhythm + optional patronage messaging).
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct BrandManifest {
    pub dtcg_version: String,
    pub brand_id: String,
    pub rhythm: BrandRhythm,
    #[serde(default)]
    pub brand: BrandIdentity,
    #[serde(default)]
    pub messaging: BrandMessaging,
}

/// Visual rhythm block: rem/grid primitives, touch floors, spacing, fluid clamps.
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct BrandRhythm {
    pub rem_base_px: f32,
    pub grid_base_px: u32,
    pub grid_half_px: u32,
    pub touch_floor_material_px: f32,
    pub touch_floor_apple_px: f32,
    pub inter_target_gap_px: f32,
    pub spacing: Vec<SpacingToken>,
    #[serde(default)]
    pub fluid: Vec<FluidToken>,
}

/// Discrete spacing token whose `px` must land on the 4px/8px grid.
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct SpacingToken {
    pub name: String,
    pub px: f32,
}

/// Fluid `clamp(min, preferred, max)` token with an explicit `cqi` slope.
///
/// `preferred` must be a positive `<number>cqi` expression. `slope_cqi` is the
/// numeric coefficient of that preferred track and must be strictly positive.
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct FluidToken {
    pub name: String,
    pub min_px: f32,
    /// Preferred track — must parse as a positive `<number>cqi` expression.
    pub preferred: String,
    pub max_px: f32,
    /// Explicit clamp slope in container-query-inline (`cqi`) units.
    pub slope_cqi: f32,
}

/// Machine-readable admit receipt for a valid brand rhythm.
#[derive(Debug, Clone, PartialEq)]
pub struct BrandReceipt {
    pub brand_id: String,
    pub dtcg_version: String,
    pub spacing_count: usize,
    pub fluid_count: usize,
}

/// Typed refusals for brand rhythm law failures.
#[derive(Debug, Clone, PartialEq)]
pub enum BrandRefusal {
    Parse(String),
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
    /// `slope_cqi` is non-finite or ≤ 0.
    InvalidClampSlope {
        token: String,
        slope_cqi: f32,
    },
    /// `preferred` is not a well-formed positive `<number>cqi` expression.
    InvalidCqiPreferred {
        token: String,
        preferred: String,
    },
    /// Explicit `slope_cqi` disagrees with the coefficient of `preferred`.
    ClampSlopeMismatch {
        token: String,
        slope_cqi: f32,
        preferred_cqi: f32,
    },
    /// `min_px` exceeds `max_px` — inverted / non-monotonic clamp.
    InvertedClamp {
        token: String,
        min_px: f32,
        max_px: f32,
    },
}

/// Parse a `*.brand.toml` document from UTF-8 text.
pub fn parse_brand_toml(raw: &str) -> Result<BrandManifest, BrandRefusal> {
    toml::from_str(raw).map_err(|e| BrandRefusal::Parse(e.to_string()))
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
        admit_fluid_token(fluid, rhythm.grid_half_px)?;
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

/// Parse a preferred track as a positive `<number>cqi` coefficient.
pub fn parse_cqi_slope(preferred: &str) -> Option<f32> {
    let trimmed = preferred.trim();
    let coeff = trimmed.strip_suffix("cqi")?.trim_end();
    if coeff.is_empty() {
        return None;
    }
    let value: f32 = coeff.parse().ok()?;
    if value.is_finite() && value > 0.0 {
        Some(value)
    } else {
        None
    }
}

fn admit_fluid_token(fluid: &FluidToken, grid_half_px: u32) -> Result<(), BrandRefusal> {
    if !fluid.slope_cqi.is_finite() || !(fluid.slope_cqi > 0.0) {
        return Err(BrandRefusal::InvalidClampSlope {
            token: fluid.name.clone(),
            slope_cqi: fluid.slope_cqi,
        });
    }

    let preferred_cqi = match parse_cqi_slope(&fluid.preferred) {
        Some(v) => v,
        None => {
            return Err(BrandRefusal::InvalidCqiPreferred {
                token: fluid.name.clone(),
                preferred: fluid.preferred.clone(),
            });
        }
    };

    if (fluid.slope_cqi - preferred_cqi).abs()
        > f32::EPSILON * 8.0_f32.max(fluid.slope_cqi.abs()).max(preferred_cqi.abs())
    {
        return Err(BrandRefusal::ClampSlopeMismatch {
            token: fluid.name.clone(),
            slope_cqi: fluid.slope_cqi,
            preferred_cqi,
        });
    }

    if fluid.min_px > fluid.max_px + f32::EPSILON {
        return Err(BrandRefusal::InvertedClamp {
            token: fluid.name.clone(),
            min_px: fluid.min_px,
            max_px: fluid.max_px,
        });
    }

    if !is_on_grid(fluid.min_px, grid_half_px) {
        return Err(BrandRefusal::OffGridFluid {
            token: fluid.name.clone(),
            field: "min_px",
            value_px: fluid.min_px,
            grid_half_px,
        });
    }
    if !is_on_grid(fluid.max_px, grid_half_px) {
        return Err(BrandRefusal::OffGridFluid {
            token: fluid.name.clone(),
            field: "max_px",
            value_px: fluid.max_px,
            grid_half_px,
        });
    }

    Ok(())
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
