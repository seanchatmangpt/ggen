//! TCPS Chicago-TDD 8^4 combinatorial innovation fabric.
//!
//! 8 production layers × 8 TCPS doctrines × 8 execution surfaces ×
//! 8 standing states = 4,096 unique scenario cells.
//!
//! The full cube is traversed by a cyclic 12-bit Gray path. Four coverage
//! rails provide strength 1/2/3/4 at 8/64/512/4,096 cells. Every cell is bound
//! to one of the 43 real checked-in TCPS artifacts; no collaborator is mocked.

#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::expect_used)]
#![allow(clippy::panic)]
#![allow(clippy::too_many_lines)]

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};

use chicago_tdd_tools::core::contract::{
    ResourceEnvelope, TestContract, TestContractRegistry,
};
use chicago_tdd_tools::core::fixture::TestFixture;
use tempfile::TempDir;

const WIDTH: usize = 8;
const DIMENSIONS: usize = 4;
const CELL_COUNT: usize = 4_096;

const AXIS_NAMES: [&str; DIMENSIONS] = [
    "production-layer",
    "tcps-doctrine",
    "execution-surface",
    "standing-state",
];

const AXES: [[&str; WIDTH]; DIMENSIONS] = [
    [
        "meaning",
        "flow",
        "quality",
        "safety",
        "authority",
        "runtime",
        "release",
        "evidence",
    ],
    [
        "standard-work",
        "jidoka",
        "just-in-time",
        "heijunka",
        "kanban",
        "poka-yoke",
        "genchi-genbutsu",
        "kaizen",
    ],
    [
        "source",
        "native",
        "no-std",
        "cli",
        "ffi",
        "wasm",
        "packaging",
        "evidence",
    ],
    [
        "admitted",
        "selected",
        "authorized",
        "actuated",
        "receipted",
        "replayed",
        "refused",
        "unsupported",
    ],
];

const INVARIANTS: &[&str] = &[
    "exhaustive",
    "orthogonal",
    "gray-adjacent",
    "marginally-balanced",
    "pairwise-balanced",
    "triple-wise-balanced",
    "real-witness-bound",
    "replay-stable",
];

const LAYER_COVERAGE: &[&str] = &[
    "layer.meaning",
    "layer.flow",
    "layer.quality",
    "layer.safety",
    "layer.authority",
    "layer.runtime",
    "layer.release",
    "layer.evidence",
];
const DOCTRINE_COVERAGE: &[&str] = &[
    "doctrine.standard-work",
    "doctrine.jidoka",
    "doctrine.just-in-time",
    "doctrine.heijunka",
    "doctrine.kanban",
    "doctrine.poka-yoke",
    "doctrine.genchi-genbutsu",
    "doctrine.kaizen",
];
const SURFACE_COVERAGE: &[&str] = &[
    "surface.source",
    "surface.native",
    "surface.no-std",
    "surface.cli",
    "surface.ffi",
    "surface.wasm",
    "surface.packaging",
    "surface.evidence",
];
const STANDING_COVERAGE: &[&str] = &[
    "standing.admitted",
    "standing.selected",
    "standing.authorized",
    "standing.actuated",
    "standing.receipted",
    "standing.replayed",
    "standing.refused",
    "standing.unsupported",
];
const ALL_COVERAGE: &[&str] = &[
    "layer.meaning",
    "layer.flow",
    "layer.quality",
    "layer.safety",
    "layer.authority",
    "layer.runtime",
    "layer.release",
    "layer.evidence",
    "doctrine.standard-work",
    "doctrine.jidoka",
    "doctrine.just-in-time",
    "doctrine.heijunka",
    "doctrine.kanban",
    "doctrine.poka-yoke",
    "doctrine.genchi-genbutsu",
    "doctrine.kaizen",
    "surface.source",
    "surface.native",
    "surface.no-std",
    "surface.cli",
    "surface.ffi",
    "surface.wasm",
    "surface.packaging",
    "surface.evidence",
    "standing.admitted",
    "standing.selected",
    "standing.authorized",
    "standing.actuated",
    "standing.receipted",
    "standing.replayed",
    "standing.refused",
    "standing.unsupported",
    "hypercube.8pow4",
];

const ENVELOPE: ResourceEnvelope =
    ResourceEnvelope::new(u64::MAX, u64::MAX, false, true, false);
const CONTRACTS: &[TestContract] = &[
    TestContract::new(
        "tcps-8pow4/layers",
        LAYER_COVERAGE,
        INVARIANTS,
        ENVELOPE,
        &["real-filesystem"],
    ),
    TestContract::new(
        "tcps-8pow4/doctrines",
        DOCTRINE_COVERAGE,
        INVARIANTS,
        ENVELOPE,
        &["real-filesystem"],
    ),
    TestContract::new(
        "tcps-8pow4/surfaces",
        SURFACE_COVERAGE,
        INVARIANTS,
        ENVELOPE,
        &["real-filesystem"],
    ),
    TestContract::new(
        "tcps-8pow4/standing",
        STANDING_COVERAGE,
        INVARIANTS,
        ENVELOPE,
        &["real-filesystem"],
    ),
    TestContract::new(
        "tcps-8pow4/full",
        &["hypercube.8pow4"],
        INVARIANTS,
        ENVELOPE,
        &["real-filesystem"],
    ),
];

const WITNESSES: [(&str, &str); 43] = [
    ("core.vocabulary", "src/語彙.rs"),
    ("core.origin", "src/原点.rs"),
    ("core.lineage", "src/系譜.rs"),
    ("core.quality", "src/品質.rs"),
    ("core.standard-work", "src/標準作業.rs"),
    ("core.jidoka", "src/自働化.rs"),
    ("core.kanban", "src/かんばん.rs"),
    ("core.just-in-time", "src/必要時生産.rs"),
    ("core.heijunka", "src/平準化.rs"),
    ("core.andon", "src/アンドン.rs"),
    ("core.kaizen", "src/改善.rs"),
    ("core.receipt", "src/受領証.rs"),
    ("core.crypto-digest", "src/暗号要約.rs"),
    ("core.auto-select", "src/自動選択.rs"),
    ("core.blue-river-dam", "src/青い川のダム.rs"),
    ("core.whole-system", "src/全体.rs"),
    ("core.canonical", "src/正準.rs"),
    ("core.genchi-genbutsu", "src/現地現物.rs"),
    ("core.poka-yoke", "src/ポカヨケ.rs"),
    ("core.five-whys", "src/五回なぜ.rs"),
    ("core.human-centered", "src/人間中心.rs"),
    ("core.muri-mura-muda", "src/ムリムラムダ.rs"),
    ("core.safety", "src/安全.rs"),
    ("core.takt", "src/タクト.rs"),
    ("core.value-stream", "src/価値流.rs"),
    ("core.process-capability", "src/工程能力.rs"),
    ("runtime.std", "crates/tcps-std/src/lib.rs"),
    ("runtime.ffi", "crates/tcps-ffi/src/lib.rs"),
    ("runtime.wasm", "crates/tcps-wasm/src/lib.rs"),
    ("runtime.cli", "crates/tcps-cli/src/main.rs"),
    ("release.ci", "infra/ci/tier1.yml"),
    ("release.targets", "infra/targets/tier1.txt"),
    ("release.packaging.deb", "infra/packaging/deb/control.in"),
    ("release.packaging.rpm", "infra/packaging/rpm/tcps.spec.in"),
    ("release.packaging.npm", "infra/packaging/npm/package.json.in"),
    (
        "release.packaging.nuget",
        "infra/packaging/nuget/tcps.nuspec.in",
    ),
    (
        "evidence.sbom.cyclonedx",
        "infra/evidence/source/tcps-v26.7.19.cdx.json",
    ),
    (
        "evidence.sbom.spdx",
        "infra/evidence/source/tcps-v26.7.19.spdx.json",
    ),
    (
        "evidence.provenance.in-toto",
        "infra/evidence/source/tcps-v26.7.19.intoto.jsonl",
    ),
    ("evidence.source-manifest", "infra/SOURCE_MANIFEST.sha256"),
    ("manufacture.lifecycle", "infra/tools/lifecycle.py"),
    (
        "manufacture.reproducible-build",
        "infra/scripts/reproducible-build.sh",
    ),
    ("manufacture.ggen-fixed-point", "ggen.lock"),
];

#[derive(Debug, Clone, PartialEq, Eq)]
struct Cell {
    ordinal: u16,
    gray: u16,
    coordinates: [u8; DIMENSIONS],
    id: String,
    witness: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Rail {
    strength: u8,
    coordinates: Vec<[u8; DIMENSIONS]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Standing {
    Accepted,
    Refused,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Probe {
    capability: &'static str,
    standing: Standing,
    observation: String,
    fingerprint: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Discharge {
    cell_id: String,
    witness: &'static str,
    evidence: u64,
    leaf: u64,
}

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn hash(bytes: &[u8]) -> u64 {
    let mut value = 0xcbf2_9ce4_8422_2325_u64;
    for byte in bytes {
        value ^= u64::from(*byte);
        value = value.wrapping_mul(0x0000_0100_0000_01b3);
    }
    value
}

const fn gray(binary: u16) -> u16 {
    binary ^ (binary >> 1)
}

fn ungray(mut value: u16) -> u16 {
    let mut binary = 0_u16;
    while value != 0 {
        binary ^= value;
        value >>= 1;
    }
    binary
}

const fn coordinates(value: u16) -> [u8; DIMENSIONS] {
    [
        (value & 7) as u8,
        ((value >> 3) & 7) as u8,
        ((value >> 6) & 7) as u8,
        ((value >> 9) & 7) as u8,
    ]
}

const fn packed(coords: [u8; DIMENSIONS]) -> u16 {
    coords[0] as u16
        | ((coords[1] as u16) << 3)
        | ((coords[2] as u16) << 6)
        | ((coords[3] as u16) << 9)
}

fn validate_coordinates(coords: [u8; DIMENSIONS]) -> Result<(), String> {
    for (axis, state) in coords.iter().copied().enumerate() {
        if usize::from(state) >= WIDTH {
            return Err(format!(
                "INVALID_AXIS_STATE:{}:{state}",
                AXIS_NAMES[axis]
            ));
        }
    }
    Ok(())
}

fn identity(coords: [u8; DIMENSIONS]) -> String {
    format!(
        "h4:{}:{}:{}:{}",
        AXES[0][usize::from(coords[0])],
        AXES[1][usize::from(coords[1])],
        AXES[2][usize::from(coords[2])],
        AXES[3][usize::from(coords[3])],
    )
}

fn cube() -> Vec<Cell> {
    (0..CELL_COUNT)
        .map(|ordinal| {
            let ordinal = ordinal as u16;
            let gray = gray(ordinal);
            let coordinates = coordinates(gray);
            Cell {
                ordinal,
                gray,
                coordinates,
                id: identity(coordinates),
                witness: WITNESSES[usize::from(ordinal) % WITNESSES.len()].0,
            }
        })
        .collect()
}

const fn gf8_mul(mut left: u8, mut right: u8) -> u8 {
    let mut product = 0_u8;
    let mut step = 0;
    while step < 3 {
        if right & 1 != 0 {
            product ^= left;
        }
        let carry = left & 4;
        left <<= 1;
        if carry != 0 {
            left ^= 0b1011;
        }
        right >>= 1;
        step += 1;
    }
    product & 7
}

fn rails() -> Vec<Rail> {
    let strength_one = (0..WIDTH)
        .map(|state| [state as u8; DIMENSIONS])
        .collect();

    let mut strength_two = Vec::with_capacity(WIDTH.pow(2));
    for x in 0..WIDTH as u8 {
        for y in 0..WIDTH as u8 {
            strength_two.push([x, y, x ^ y, x ^ gf8_mul(2, y)]);
        }
    }

    let mut strength_three = Vec::with_capacity(WIDTH.pow(3));
    for x in 0..WIDTH as u8 {
        for y in 0..WIDTH as u8 {
            for z in 0..WIDTH as u8 {
                strength_three.push([x, y, z, x ^ y ^ z]);
            }
        }
    }

    vec![
        Rail {
            strength: 1,
            coordinates: strength_one,
        },
        Rail {
            strength: 2,
            coordinates: strength_two,
        },
        Rail {
            strength: 3,
            coordinates: strength_three,
        },
        Rail {
            strength: 4,
            coordinates: cube().into_iter().map(|cell| cell.coordinates).collect(),
        },
    ]
}

fn validate_rail(rail: &Rail) -> Result<(), String> {
    let expected = WIDTH.pow(u32::from(rail.strength));
    if rail.coordinates.len() != expected {
        return Err(format!("RAIL_CARDINALITY:{}:{}", rail.strength, expected));
    }
    for coords in &rail.coordinates {
        validate_coordinates(*coords)?;
    }
    if rail.coordinates.iter().copied().collect::<BTreeSet<_>>().len()
        != rail.coordinates.len()
    {
        return Err(format!("RAIL_DUPLICATE:{}", rail.strength));
    }
    Ok(())
}

fn probe(root: &Path, witness: (&'static str, &'static str)) -> Probe {
    match fs::read(root.join(witness.1)) {
        Ok(bytes) => Probe {
            capability: witness.0,
            standing: Standing::Accepted,
            observation: witness.1.to_string(),
            fingerprint: hash(&bytes),
        },
        Err(error) => Probe {
            capability: witness.0,
            standing: Standing::Refused,
            observation: format!("MISSING_ARTIFACT:{}:{error}", witness.1),
            fingerprint: 0,
        },
    }
}

fn probes(root: &Path) -> Vec<Probe> {
    WITNESSES.iter().copied().map(|item| probe(root, item)).collect()
}

fn discharge(cells: &[Cell], probes: &[Probe]) -> Result<Vec<Discharge>, String> {
    let accepted: BTreeMap<_, _> = probes
        .iter()
        .filter(|probe| probe.standing == Standing::Accepted)
        .map(|probe| (probe.capability, probe.fingerprint))
        .collect();

    cells
        .iter()
        .map(|cell| {
            let evidence = accepted.get(cell.witness).copied().ok_or_else(|| {
                format!("UNDISCHARGED_HYPERCUBE_CELL:{}:{}", cell.id, cell.witness)
            })?;
            let leaf = hash(
                format!(
                    "{}|{}|{}|{}|{evidence:016x}",
                    cell.ordinal, cell.gray, cell.id, cell.witness
                )
                .as_bytes(),
            );
            Ok(Discharge {
                cell_id: cell.id.clone(),
                witness: cell.witness,
                evidence,
                leaf,
            })
        })
        .collect()
}

fn pair_hash(left: u64, right: u64) -> u64 {
    let mut bytes = [0_u8; 16];
    bytes[..8].copy_from_slice(&left.to_le_bytes());
    bytes[8..].copy_from_slice(&right.to_le_bytes());
    hash(&bytes)
}

fn merkle(discharges: &[Discharge]) -> u64 {
    assert!(!discharges.is_empty() && discharges.len().is_power_of_two());
    let mut level: Vec<_> = discharges.iter().map(|item| item.leaf).collect();
    while level.len() > 1 {
        level = level
            .chunks_exact(2)
            .map(|pair| pair_hash(pair[0], pair[1]))
            .collect();
    }
    level[0]
}

fn loads(cells: &[Cell]) -> BTreeMap<&'static str, usize> {
    let mut result = BTreeMap::new();
    for cell in cells {
        *result.entry(cell.witness).or_insert(0) += 1;
    }
    result
}

fn shard(id: &str, count: usize) -> usize {
    assert!(count > 0);
    (0..count)
        .max_by_key(|owner| hash(format!("{id}#{owner}").as_bytes()))
        .expect("nonempty shard population")
}

fn changed_axes(left: &Cell, right: &Cell) -> usize {
    left.coordinates
        .iter()
        .zip(right.coordinates.iter())
        .filter(|(left, right)| left != right)
        .count()
}

fn receipt_json(cells: &[Cell], probes: &[Probe], discharges: &[Discharge], time: &str) -> String {
    let loads = loads(cells);
    let rails = rails();
    format!(
        concat!(
            "{{\n",
            "  \"schema\": \"tcps-chicago-tdd-8pow4/v1\",\n",
            "  \"recorded_at\": \"{}\",\n",
            "  \"axes\": 4,\n",
            "  \"states_per_axis\": 8,\n",
            "  \"cells\": {},\n",
            "  \"probes\": {},\n",
            "  \"accepted_probes\": {},\n",
            "  \"refused_probes\": {},\n",
            "  \"discharges\": {},\n",
            "  \"coverage_rails\": [{},{},{},{}],\n",
            "  \"min_witness_load\": {},\n",
            "  \"max_witness_load\": {},\n",
            "  \"merkle_root\": \"{:016x}\"\n",
            "}}\n"
        ),
        time,
        cells.len(),
        probes.len(),
        probes
            .iter()
            .filter(|probe| probe.standing == Standing::Accepted)
            .count(),
        probes
            .iter()
            .filter(|probe| probe.standing == Standing::Refused)
            .count(),
        discharges.len(),
        rails[0].coordinates.len(),
        rails[1].coordinates.len(),
        rails[2].coordinates.len(),
        rails[3].coordinates.len(),
        loads.values().copied().min().unwrap_or(0),
        loads.values().copied().max().unwrap_or(0),
        merkle(discharges),
    )
}

fn turtle(cells: &[Cell]) -> String {
    let mut out = String::from("@prefix h4: <urn:tcps:8pow4:> .\n\n");
    for cell in cells {
        writeln!(
            out,
            "h4:cell-{} a h4:ScenarioCell ; h4:identity \"{}\" ; h4:witness \"{}\" .",
            cell.ordinal, cell.id, cell.witness
        )
        .expect("write Turtle");
    }
    out
}

fn ocel(discharges: &[Discharge]) -> String {
    let mut out =
        String::from("{\n  \"ocel:global-log\":{\"ocel:version\":\"2.0\"},\n  \"ocel:events\":[\n");
    for (index, item) in discharges.iter().enumerate() {
        if index > 0 {
            out.push_str(",\n");
        }
        write!(
            out,
            "    {{\"ocel:eid\":\"h4-{index}\",\"ocel:activity\":\"DischargeHypercubeCell\",\"cell\":\"{}\",\"witness\":\"{}\",\"evidence\":\"{:016x}\",\"leaf\":\"{:016x}\"}}",
            item.cell_id, item.witness, item.evidence, item.leaf
        )
        .expect("write OCEL");
    }
    out.push_str("\n  ]\n}\n");
    out
}

fn plan(cells: &[Cell]) -> String {
    let mut out =
        String::from("ordinal\tgray\tlayer\tdoctrine\tsurface\tstanding\twitness\n");
    for cell in cells {
        writeln!(
            out,
            "{}\t{}\t{}\t{}\t{}\t{}\t{}",
            cell.ordinal,
            cell.gray,
            AXES[0][usize::from(cell.coordinates[0])],
            AXES[1][usize::from(cell.coordinates[1])],
            AXES[2][usize::from(cell.coordinates[2])],
            AXES[3][usize::from(cell.coordinates[3])],
            cell.witness
        )
        .expect("write plan");
    }
    out
}

fn rail_plan(rails: &[Rail]) -> String {
    let mut out = String::from("strength\trow\tlayer\tdoctrine\tsurface\tstanding\n");
    for rail in rails {
        for (row, coords) in rail.coordinates.iter().enumerate() {
            writeln!(
                out,
                "{}\t{}\t{}\t{}\t{}\t{}",
                rail.strength,
                row,
                AXES[0][usize::from(coords[0])],
                AXES[1][usize::from(coords[1])],
                AXES[2][usize::from(coords[2])],
                AXES[3][usize::from(coords[3])]
            )
            .expect("write rail");
        }
    }
    out
}

chicago_tdd_tools::test!(constitution_and_contract_registry_are_closed, {
    assert_eq!(AXES.len(), DIMENSIONS);
    assert!(AXES.iter().all(|axis| axis.len() == WIDTH));
    assert_eq!(WITNESSES.len(), 43);

    let registry = TestContractRegistry::new(CONTRACTS);
    assert_eq!(registry.len(), 5);
    assert!(registry.uncovered_modules(ALL_COVERAGE).is_empty());
    assert!(registry.uncovered_invariants(INVARIANTS).is_empty());
});

chicago_tdd_tools::test!(compiler_emits_4096_unique_valid_cells, {
    let cells = cube();
    assert_eq!(cells.len(), CELL_COUNT);
    assert_eq!(
        cells.iter().map(|cell| cell.id.as_str()).collect::<BTreeSet<_>>().len(),
        CELL_COUNT
    );
    assert_eq!(
        cells.iter().map(|cell| cell.coordinates).collect::<BTreeSet<_>>().len(),
        CELL_COUNT
    );
    assert!(cells.iter().all(|cell| {
        validate_coordinates(cell.coordinates).is_ok()
            && cell.gray == gray(cell.ordinal)
            && packed(cell.coordinates) == cell.gray
            && ungray(cell.gray) == cell.ordinal
            && identity(cell.coordinates) == cell.id
    }));
});

chicago_tdd_tools::test!(coverage_ladder_is_8_64_512_4096, {
    let rails = rails();
    assert_eq!(rails.iter().map(|rail| rail.coordinates.len()).collect::<Vec<_>>(), vec![8, 64, 512, 4_096]);
    assert!(rails.iter().all(|rail| validate_rail(rail).is_ok()));
});

chicago_tdd_tools::test!(gf8_pairwise_rail_covers_every_pair_once, {
    for multiplier in 1..WIDTH as u8 {
        assert_eq!(
            (0..WIDTH as u8)
                .map(|value| gf8_mul(multiplier, value))
                .collect::<BTreeSet<_>>()
                .len(),
            WIDTH
        );
    }

    let compiled = rails();
    let rail = &compiled[1];
    for left in 0..DIMENSIONS {
        for right in (left + 1)..DIMENSIONS {
            assert_eq!(
                rail.coordinates
                    .iter()
                    .map(|coords| (coords[left], coords[right]))
                    .collect::<BTreeSet<_>>()
                    .len(),
                WIDTH.pow(2)
            );
        }
    }
});

chicago_tdd_tools::test!(triple_rail_covers_every_triple_once, {
    let compiled = rails();
    let rail = &compiled[2];
    for omitted in 0..DIMENSIONS {
        let axes: Vec<_> = (0..DIMENSIONS).filter(|axis| *axis != omitted).collect();
        assert_eq!(
            rail.coordinates
                .iter()
                .map(|coords| [coords[axes[0]], coords[axes[1]], coords[axes[2]]])
                .collect::<BTreeSet<_>>()
                .len(),
            WIDTH.pow(3)
        );
    }
});

chicago_tdd_tools::test!(full_cube_is_balanced_at_every_projection_order, {
    let cells = cube();

    for axis in 0..DIMENSIONS {
        let mut counts = [0_usize; WIDTH];
        for cell in &cells {
            counts[usize::from(cell.coordinates[axis])] += 1;
        }
        assert_eq!(counts, [512; WIDTH]);
    }

    for left in 0..DIMENSIONS {
        for right in (left + 1)..DIMENSIONS {
            let mut counts = [[0_usize; WIDTH]; WIDTH];
            for cell in &cells {
                counts[usize::from(cell.coordinates[left])]
                    [usize::from(cell.coordinates[right])] += 1;
            }
            assert!(counts.into_iter().flatten().all(|count| count == 64));
        }
    }

    for omitted in 0..DIMENSIONS {
        let axes: Vec<_> = (0..DIMENSIONS).filter(|axis| *axis != omitted).collect();
        let mut counts = BTreeMap::new();
        for cell in &cells {
            *counts
                .entry([
                    cell.coordinates[axes[0]],
                    cell.coordinates[axes[1]],
                    cell.coordinates[axes[2]],
                ])
                .or_insert(0_usize) += 1;
        }
        assert_eq!(counts.len(), WIDTH.pow(3));
        assert!(counts.values().all(|count| *count == 8));
    }
});

chicago_tdd_tools::test!(gray_traversal_is_cyclic_and_changes_one_axis, {
    let cells = cube();
    for pair in cells.windows(2) {
        assert_eq!((pair[0].gray ^ pair[1].gray).count_ones(), 1);
        assert_eq!(changed_axes(&pair[0], &pair[1]), 1);
    }
    assert_eq!((cells[0].gray ^ cells[CELL_COUNT - 1].gray).count_ones(), 1);
    assert_eq!(changed_axes(&cells[0], &cells[CELL_COUNT - 1]), 1);
});

chicago_tdd_tools::test!(all_real_witnesses_discharge_all_cells, {
    let fixture = TestFixture::with_data(root());
    let probes = probes(fixture.inner());
    let refused: Vec<_> = probes
        .iter()
        .filter(|probe| probe.standing == Standing::Refused)
        .collect();
    assert!(refused.is_empty(), "refused witnesses: {refused:#?}");

    let cells = cube();
    let discharges = discharge(&cells, &probes).expect("discharge");
    assert_eq!(discharges.len(), CELL_COUNT);

    let loads = loads(&cells);
    assert_eq!(loads.len(), 43);
    assert_eq!(loads.values().copied().min(), Some(95));
    assert_eq!(loads.values().copied().max(), Some(96));
});

chicago_tdd_tools::test!(eight_way_sharding_is_deterministic_and_total, {
    let cells = cube();
    let first: Vec<_> = cells.iter().map(|cell| shard(&cell.id, 8)).collect();
    let second: Vec<_> = cells.iter().map(|cell| shard(&cell.id, 8)).collect();
    assert_eq!(first, second);
    for owner in 0..8 {
        assert!(first.contains(&owner));
    }
});

chicago_tdd_tools::test!(receipt_root_is_replay_stable_and_mutation_sensitive, {
    let cells = cube();
    let probes = probes(&root());
    let discharges = discharge(&cells, &probes).expect("discharge");

    let first = receipt_json(&cells, &probes, &discharges, "2026-07-21T00:00:00Z");
    let second = receipt_json(&cells, &probes, &discharges, "2036-07-21T00:00:00Z");
    assert_ne!(first, second);

    let root = merkle(&discharges);
    let marker = format!("\"merkle_root\": \"{root:016x}\"");
    assert!(first.contains(&marker) && second.contains(&marker));

    let mut mutated = discharges.clone();
    mutated[2_048].leaf ^= 1;
    assert_ne!(merkle(&mutated), root);
});

chicago_tdd_tools::test!(invalid_state_and_missing_witness_refuse_loudly, {
    assert!(matches!(
        validate_coordinates([8, 0, 0, 0]),
        Err(message) if message.starts_with("INVALID_AXIS_STATE:")
    ));

    let cells = cube();
    let mut probes = probes(&root());
    probes.retain(|probe| probe.capability != WITNESSES[0].0);
    assert!(matches!(
        discharge(&cells, &probes),
        Err(message) if message.starts_with("UNDISCHARGED_HYPERCUBE_CELL:")
    ));
});

chicago_tdd_tools::test!(evidence_projections_preserve_all_4096_identities, {
    let cells = cube();
    let probes = probes(&root());
    let discharges = discharge(&cells, &probes).expect("discharge");
    let turtle = turtle(&cells);
    let ocel = ocel(&discharges);
    let plan = plan(&cells);

    assert_eq!(turtle.matches("a h4:ScenarioCell").count(), CELL_COUNT);
    assert_eq!(ocel.matches("DischargeHypercubeCell").count(), CELL_COUNT);
    assert_eq!(plan.lines().count(), CELL_COUNT + 1);
    assert!(turtle.contains("h4:meaning:standard-work:source:admitted"));
    assert!(ocel.contains("manufacture.ggen-fixed-point"));
});

chicago_tdd_tools::test!(artifact_bundle_is_complete_and_isolated, {
    let cells = cube();
    let probes = probes(&root());
    let discharges = discharge(&cells, &probes).expect("discharge");
    let output = TempDir::new().expect("tempdir");

    let artifacts = [
        (
            "hypercube-receipt.json",
            receipt_json(&cells, &probes, &discharges, "2026-07-21T00:00:00Z"),
        ),
        ("hypercube.ttl", turtle(&cells)),
        ("hypercube.ocel.json", ocel(&discharges)),
        ("hypercube-plan.tsv", plan(&cells)),
        ("coverage-ladder.tsv", rail_plan(&rails())),
        ("hypercube-merkle.txt", format!("{:016x}\n", merkle(&discharges))),
    ];
    for (name, content) in artifacts {
        fs::write(output.path().join(name), content).expect("write artifact");
        assert!(fs::metadata(output.path().join(name)).expect("metadata").len() > 0);
    }
});
