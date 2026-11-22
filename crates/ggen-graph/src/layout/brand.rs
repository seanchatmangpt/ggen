//! DTCG 2025.10-aligned `*.brand.toml` rhythm schema (ECOSYSTEM-SPEC-27.1.1 D4 Gate-1 slice).
//!
//! Parses human-authored visual-rhythm manifests and emits typed refusals for
//! touch floors, inter-target gaps, off-grid spacing, and fluid `clamp()` /
//! `cqi` slope law. Optional brand / patronage messaging slots are accepted as
//! plain TOML strings (no private ontology namespaces). Full brand token
//! inventories (color/type/CASTLE) remain Gate 2+.
//!
//! Also exposes an in-memory OEM **hot-swap** fence ([`seed_hot_swap_slot`] /
//! [`hot_swap_brand`]) that admits Fourth Watch ↔ CindyCat transitions under
//! rhythm law. Standing ceiling is [`HotSwapStanding::PartialAlive`] — never
//! production runtime / CASTLE publish `ALIVE`. Production theme promote is
//! fail-closed via [`promote_production_theme`] → [`REFUSED_PRODUCTION`].

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

// ---------------------------------------------------------------------------
// Brand.toml hot-swap fence (Gate-1 CONSTRUCT slot — PARTIAL_ALIVE ceiling)
// ---------------------------------------------------------------------------
//
// Swaps the active admitted OEM `*.brand.toml` between already-admitted rhythm
// stubs (Fourth Watch ↔ CindyCat). This is an in-memory admit/refuse surface
// only: it does **not** claim production runtime theme swap, CASTLE publish,
// full DTCG color/type inventory, or crown `ALIVE`. Mirror of the AS-01 seat
// fence pattern — presence of a lawful swap receipt is not ambient DO.

/// Closed Gate-1 OEM brand ids eligible for hot-swap (ECOSYSTEM-SPEC D4 stubs).
pub const ADMITTED_OEM_BRAND_IDS: &[&str] = &["fourth-watch", "cindycat"];

/// Standing ceiling for the in-repo hot-swap fence (never production `ALIVE`).
pub const HOT_SWAP_STANDING_CEILING: &str = "PARTIAL_ALIVE";

/// Standing recorded on a successful hot-swap / seed receipt.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HotSwapStanding {
    /// In-repo admit/swap fence passed — production hot-swap remains unsupported.
    PartialAlive,
}

impl HotSwapStanding {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::PartialAlive => HOT_SWAP_STANDING_CEILING,
        }
    }
}

/// Active brand slot holding one admitted manifest (hot-swap target).
#[derive(Debug, Clone, PartialEq)]
pub struct BrandHotSwapSlot {
    active: BrandManifest,
    receipt: BrandReceipt,
}

impl BrandHotSwapSlot {
    /// Currently active OEM `brand_id`.
    pub fn brand_id(&self) -> &str {
        &self.active.brand_id
    }

    /// Last admit receipt for the active brand.
    pub fn receipt(&self) -> &BrandReceipt {
        &self.receipt
    }

    /// Borrow the active admitted manifest (rhythm + optional messaging).
    pub fn manifest(&self) -> &BrandManifest {
        &self.active
    }
}

/// Machine-readable receipt for a successful seed or hot-swap transition.
#[derive(Debug, Clone, PartialEq)]
pub struct HotSwapReceipt {
    /// Prior active brand id (`None` on first seed).
    pub from_brand_id: Option<String>,
    /// Newly active brand id.
    pub to_brand_id: String,
    /// Always [`HotSwapStanding::PartialAlive`] — never production `ALIVE`.
    pub standing: HotSwapStanding,
    /// Rhythm admit receipt for the new active brand.
    pub to: BrandReceipt,
}

/// Typed refusals for brand.toml hot-swap (CONSTRUCT fence, not DO).
#[derive(Debug, Clone, PartialEq)]
pub enum BrandHotSwapRefusal {
    /// Target failed Gate-1 rhythm admit.
    Rhythm(BrandRefusal),
    /// `brand_id` empty after parse.
    EmptyBrandId,
    /// `brand_id` is not in [`ADMITTED_OEM_BRAND_IDS`] (refuse inventing brands).
    OemBrandUnsupported { brand_id: String },
    /// Hot-swap target is the same as the active brand (no transition).
    SameBrand { brand_id: String },
}

impl BrandHotSwapRefusal {
    pub fn code(&self) -> &'static str {
        match self {
            Self::Rhythm(_) => "REFUSED_BRAND_HOTSWAP_RHYTHM",
            Self::EmptyBrandId => "REFUSED_BRAND_HOTSWAP_EMPTY_ID",
            Self::OemBrandUnsupported { .. } => "REFUSED_BRAND_HOTSWAP_OEM",
            Self::SameBrand { .. } => "REFUSED_BRAND_HOTSWAP_SAME",
        }
    }
}

fn is_admitted_oem_brand(brand_id: &str) -> bool {
    ADMITTED_OEM_BRAND_IDS.iter().any(|id| *id == brand_id)
}

fn admit_oem_manifest(raw: &str) -> Result<(BrandManifest, BrandReceipt), BrandHotSwapRefusal> {
    let manifest = parse_brand_toml(raw).map_err(BrandHotSwapRefusal::Rhythm)?;
    if manifest.brand_id.trim().is_empty() {
        return Err(BrandHotSwapRefusal::EmptyBrandId);
    }
    if !is_admitted_oem_brand(&manifest.brand_id) {
        return Err(BrandHotSwapRefusal::OemBrandUnsupported {
            brand_id: manifest.brand_id,
        });
    }
    let receipt = admit_brand(&manifest).map_err(BrandHotSwapRefusal::Rhythm)?;
    Ok((manifest, receipt))
}

/// Seed an empty hot-swap slot from an admitted OEM `*.brand.toml` body.
///
/// Standing on success is [`HotSwapStanding::PartialAlive`] only.
pub fn seed_hot_swap_slot(
    raw: &str,
) -> Result<(BrandHotSwapSlot, HotSwapReceipt), BrandHotSwapRefusal> {
    let (manifest, receipt) = admit_oem_manifest(raw)?;
    let to_brand_id = receipt.brand_id.clone();
    let slot = BrandHotSwapSlot {
        active: manifest,
        receipt: receipt.clone(),
    };
    let swap = HotSwapReceipt {
        from_brand_id: None,
        to_brand_id,
        standing: HotSwapStanding::PartialAlive,
        to: receipt,
    };
    Ok((slot, swap))
}

/// Hot-swap the active OEM brand to another admitted `*.brand.toml` body.
///
/// Refuses same-brand no-ops, unknown OEM ids, and rhythm law failures.
/// Does not write files, publish CASTLE, or authorize production theme DO.
pub fn hot_swap_brand(
    slot: &mut BrandHotSwapSlot, next_raw: &str,
) -> Result<HotSwapReceipt, BrandHotSwapRefusal> {
    let (manifest, receipt) = admit_oem_manifest(next_raw)?;
    if manifest.brand_id == slot.active.brand_id {
        return Err(BrandHotSwapRefusal::SameBrand {
            brand_id: manifest.brand_id,
        });
    }
    let from_brand_id = Some(slot.active.brand_id.clone());
    let to_brand_id = receipt.brand_id.clone();
    slot.active = manifest;
    slot.receipt = receipt.clone();
    Ok(HotSwapReceipt {
        from_brand_id,
        to_brand_id,
        standing: HotSwapStanding::PartialAlive,
        to: receipt,
    })
}

// ---------------------------------------------------------------------------
// Production theme promote fence (always REFUSED_PRODUCTION on Gate-1 path)
// ---------------------------------------------------------------------------
//
// Hot-swap + fluid-clamp courts are PARTIAL_ALIVE CONSTRUCT fences. Production
// runtime theme apply remains UNSUPPORTED until full DTCG inventory (Gate 2+)
// and a dedicated apply court are admitted. This surface fail-closes any
// attempt to promote a hot-swap receipt to production DO.

/// Standing for production runtime theme apply (never `ALIVE` from Gate-1).
pub const PRODUCTION_THEME_STANDING: &str = "UNSUPPORTED";

/// Fail-closed refusal code when CONSTRUCT hot-swap tries to authorize
/// production theme DO ([`promote_production_theme`]).
pub const REFUSED_PRODUCTION: &str = "REFUSED_PRODUCTION";

/// Standing vocabulary for the production theme surface.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProductionThemeStanding {
    /// Production runtime theme path is not admitted in this repository.
    Unsupported,
}

impl ProductionThemeStanding {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Unsupported => PRODUCTION_THEME_STANDING,
        }
    }
}

/// Typed refusal when production theme promote is attempted on Gate-1 path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProductionThemeRefusal {
    /// Hot-swap / rhythm receipts cannot authorize production theme DO.
    ProductionUnsupported { brand_id: String, detail: String },
}

impl ProductionThemeRefusal {
    pub fn code(&self) -> &'static str {
        match self {
            Self::ProductionUnsupported { .. } => REFUSED_PRODUCTION,
        }
    }
}

/// Report production theme standing — always [`Unsupported`] on Gate-1 path.
pub fn production_theme_standing() -> ProductionThemeStanding {
    ProductionThemeStanding::Unsupported
}

/// Attempt to promote a hot-swap receipt to production runtime theme.
///
/// Fail-closed: Gate-1 OEM hot-swap is CONSTRUCT [`HotSwapStanding::PartialAlive`]
/// only. Never returns `Ok` until a dedicated production theme path is admitted
/// (full DTCG inventory + runtime apply court — see
/// `docs/rfc/v27.1.1/BRAND-PRODUCTION-THEME.md`).
pub fn promote_production_theme(
    slot: &BrandHotSwapSlot,
    _receipt: &HotSwapReceipt,
) -> Result<ProductionThemeStanding, ProductionThemeRefusal> {
    Err(ProductionThemeRefusal::ProductionUnsupported {
        brand_id: slot.brand_id().to_string(),
        detail: format!(
            "brand.toml hot-swap standing is {HOT_SWAP_STANDING_CEILING}; \
             production theme remains {PRODUCTION_THEME_STANDING} \
             (see docs/rfc/v27.1.1/BRAND-PRODUCTION-THEME.md)"
        ),
    })
}

#[cfg(test)]
mod production_theme_tests {
    //! Lib-only Chicago fence: production theme promote always REFUSED_PRODUCTION.
    //!
    //! Falsifier (no oxrocksdb / integration harness):
    //! `cargo test -p ggen-graph --lib layout::brand::production_theme_tests`

    use super::*;

    /// Minimal admitted Fourth Watch rhythm body (inline — no fixture IO).
    const FOURTH_WATCH_MIN: &str = r#"
dtcg_version = "2025.10"
brand_id = "fourth-watch"

[rhythm]
rem_base_px = 16
grid_base_px = 8
grid_half_px = 4
touch_floor_material_px = 48
touch_floor_apple_px = 44
inter_target_gap_px = 8

[[rhythm.spacing]]
name = "2"
px = 8

[[rhythm.fluid]]
name = "space-fluid-sm"
min_px = 8
preferred = "2cqi"
max_px = 16
slope_cqi = 2.0
"#;

    const CINDYCAT_MIN: &str = r#"
dtcg_version = "2025.10"
brand_id = "cindycat"

[rhythm]
rem_base_px = 16
grid_base_px = 8
grid_half_px = 4
touch_floor_material_px = 48
touch_floor_apple_px = 44
inter_target_gap_px = 8

[[rhythm.spacing]]
name = "2"
px = 8

[[rhythm.fluid]]
name = "space-fluid-sm"
min_px = 8
preferred = "2cqi"
max_px = 16
slope_cqi = 2.0
"#;

    #[test]
    fn production_theme_standing_is_unsupported() {
        assert_eq!(
            production_theme_standing(),
            ProductionThemeStanding::Unsupported
        );
        assert_eq!(
            production_theme_standing().as_str(),
            PRODUCTION_THEME_STANDING
        );
        assert_ne!(production_theme_standing().as_str(), "ALIVE");
        assert_ne!(production_theme_standing().as_str(), HOT_SWAP_STANDING_CEILING);
    }

    #[test]
    fn promote_after_hot_swap_is_refused_production() {
        // Arrange — PARTIAL hot-swap succeeds
        let (mut slot, seed) =
            seed_hot_swap_slot(FOURTH_WATCH_MIN).expect("fourth-watch must seed");
        assert_eq!(seed.standing, HotSwapStanding::PartialAlive);
        let swap = hot_swap_brand(&mut slot, CINDYCAT_MIN).expect("cindycat swap admits");
        assert_eq!(swap.standing, HotSwapStanding::PartialAlive);
        assert_eq!(slot.brand_id(), "cindycat");

        // Act — production promote must fail closed
        let result = promote_production_theme(&slot, &swap);

        // Assert
        match result {
            Err(refused @ ProductionThemeRefusal::ProductionUnsupported { brand_id, detail }) => {
                assert_eq!(brand_id, "cindycat");
                assert_eq!(refused.code(), REFUSED_PRODUCTION);
                assert!(
                    detail.contains(PRODUCTION_THEME_STANDING)
                        || detail.contains("UNSUPPORTED")
                        || detail.contains("BRAND-PRODUCTION-THEME"),
                    "detail must name production UNSUPPORTED: {detail}"
                );
                assert!(
                    detail.contains(HOT_SWAP_STANDING_CEILING)
                        || detail.contains("PARTIAL_ALIVE"),
                    "detail must name hot-swap PARTIAL ceiling: {detail}"
                );
            }
            Ok(standing) => panic!("expected REFUSED_PRODUCTION, got Ok({standing:?})"),
        }
    }

    #[test]
    fn promote_on_seed_alone_is_refused_production() {
        // Arrange
        let (slot, seed) = seed_hot_swap_slot(FOURTH_WATCH_MIN).expect("seed");

        // Act
        let result = promote_production_theme(&slot, &seed);

        // Assert — seed receipt never elevates to production
        match result {
            Err(ProductionThemeRefusal::ProductionUnsupported { brand_id, .. }) => {
                assert_eq!(brand_id, "fourth-watch");
                assert_eq!(
                    ProductionThemeRefusal::ProductionUnsupported {
                        brand_id: "x".into(),
                        detail: String::new(),
                    }
                    .code(),
                    "REFUSED_PRODUCTION"
                );
            }
            other => panic!("expected ProductionUnsupported, got {other:?}"),
        }
    }
}
