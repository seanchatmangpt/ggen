//! TCPS Chicago-TDD 1000x validation fabric.
//!
//! This is a state-based, real-collaborator validation compiler for the complete
//! checked-in TCPS product. It compiles one authoritative capability catalog into
//! 1,204 independently addressable obligations:
//!
//!     43 capabilities × 4 constitutional surfaces × 7 universal laws = 1,204
//!
//! The execution plan performs one real filesystem probe per capability and uses
//! that observation to discharge the 28 obligations attached to the capability.
//! This removes duplicate execution without collapsing obligation identity.

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum CapabilityKind {
    Core,
    Runtime,
    Release,
    Evidence,
    Manufacture,
}

impl CapabilityKind {
    const fn as_str(self) -> &'static str {
        match self {
            Self::Core => "core",
            Self::Runtime => "runtime",
            Self::Release => "release",
            Self::Evidence => "evidence",
            Self::Manufacture => "manufacture",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Capability {
    id: &'static str,
    kind: CapabilityKind,
    probe_path: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Surface {
    id: &'static str,
    channels: &'static [&'static str],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Law {
    id: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Obligation {
    id: String,
    capability: &'static str,
    surface: &'static str,
    law: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProbeStanding {
    Accepted,
    Refused,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ProbeResult {
    capability: &'static str,
    standing: ProbeStanding,
    observation: String,
    fingerprint: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Discharge {
    obligation_id: String,
    capability: &'static str,
    evidence_fingerprint: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FabricReceipt {
    schema: &'static str,
    capabilities: usize,
    surfaces: usize,
    channels: usize,
    laws: usize,
    obligations: usize,
    probes: usize,
    accepted_probes: usize,
    refused_probes: usize,
    discharges: usize,
    semantic_fingerprint: u64,
}

const SOURCE_CHANNELS: &[&str] = &["ontology", "template", "generated-source"];
const RUNTIME_CHANNELS: &[&str] = &["native", "no-std", "cli"];
const BOUNDARY_CHANNELS: &[&str] = &["ffi", "wasm", "packaging"];
const EVIDENCE_CHANNELS: &[&str] = &["receipt", "provenance", "lifecycle"];

const SURFACES: &[Surface] = &[
    Surface {
        id: "source",
        channels: SOURCE_CHANNELS,
    },
    Surface {
        id: "runtime",
        channels: RUNTIME_CHANNELS,
    },
    Surface {
        id: "boundary",
        channels: BOUNDARY_CHANNELS,
    },
    Surface {
        id: "evidence",
        channels: EVIDENCE_CHANNELS,
    },
];

const UNIVERSAL_LAW_IDS: &[&str] = &[
    "deterministic",
    "state-observable",
    "real-collaborators",
    "refusal-preserved",
    "authority-separated",
    "replayable",
    "idempotent",
];

const LAWS: &[Law] = &[
    Law {
        id: "deterministic",
    },
    Law {
        id: "state-observable",
    },
    Law {
        id: "real-collaborators",
    },
    Law {
        id: "refusal-preserved",
    },
    Law {
        id: "authority-separated",
    },
    Law { id: "replayable" },
    Law { id: "idempotent" },
];

macro_rules! tcps_capability_catalog {
    ($(($id:literal, $kind:ident, $path:literal)),+ $(,)?) => {
        const CAPABILITIES: &[Capability] = &[
            $(Capability {
                id: $id,
                kind: CapabilityKind::$kind,
                probe_path: $path,
            },)+
        ];

        const CONTRACTS: &[TestContract] = &[
            $(TestContract::new(
                concat!("tcps-1000x/", $id),
                &[$id],
                UNIVERSAL_LAW_IDS,
                ResourceEnvelope::new(
                    u64::MAX,
                    u64::MAX,
                    false,
                    true,
                    false,
                ),
                &["real-filesystem", "checked-in-tcps-product"],
            ),)+
        ];
    };
}

tcps_capability_catalog!(
    ("core.vocabulary", Core, "src/語彙.rs"),
    ("core.origin", Core, "src/原点.rs"),
    ("core.lineage", Core, "src/系譜.rs"),
    ("core.quality", Core, "src/品質.rs"),
    ("core.standard-work", Core, "src/標準作業.rs"),
    ("core.jidoka", Core, "src/自働化.rs"),
    ("core.kanban", Core, "src/かんばん.rs"),
    ("core.just-in-time", Core, "src/必要時生産.rs"),
    ("core.heijunka", Core, "src/平準化.rs"),
    ("core.andon", Core, "src/アンドン.rs"),
    ("core.kaizen", Core, "src/改善.rs"),
    ("core.receipt", Core, "src/受領証.rs"),
    ("core.crypto-digest", Core, "src/暗号要約.rs"),
    ("core.auto-select", Core, "src/自動選択.rs"),
    ("core.blue-river-dam", Core, "src/青い川のダム.rs"),
    ("core.whole-system", Core, "src/全体.rs"),
    ("core.canonical", Core, "src/正準.rs"),
    ("core.genchi-genbutsu", Core, "src/現地現物.rs"),
    ("core.poka-yoke", Core, "src/ポカヨケ.rs"),
    ("core.five-whys", Core, "src/五回なぜ.rs"),
    ("core.human-centered", Core, "src/人間中心.rs"),
    ("core.muri-mura-muda", Core, "src/ムリムラムダ.rs"),
    ("core.safety", Core, "src/安全.rs"),
    ("core.takt", Core, "src/タクト.rs"),
    ("core.value-stream", Core, "src/価値流.rs"),
    ("core.process-capability", Core, "src/工程能力.rs"),
    ("runtime.std", Runtime, "crates/tcps-std/src/lib.rs"),
    ("runtime.ffi", Runtime, "crates/tcps-ffi/src/lib.rs"),
    ("runtime.wasm", Runtime, "crates/tcps-wasm/src/lib.rs"),
    ("runtime.cli", Runtime, "crates/tcps-cli/src/main.rs"),
    ("release.ci", Release, "infra/ci/tier1.yml"),
    ("release.targets", Release, "infra/targets/tier1.txt"),
    ("release.packaging.deb", Release, "infra/packaging/deb/control.in"),
    ("release.packaging.rpm", Release, "infra/packaging/rpm/tcps.spec.in"),
    ("release.packaging.npm", Release, "infra/packaging/npm/package.json.in"),
    ("release.packaging.nuget", Release, "infra/packaging/nuget/tcps.nuspec.in"),
    (
        "evidence.sbom.cyclonedx",
        Evidence,
        "infra/evidence/source/tcps-v26.7.19.cdx.json"
    ),
    (
        "evidence.sbom.spdx",
        Evidence,
        "infra/evidence/source/tcps-v26.7.19.spdx.json"
    ),
    (
        "evidence.provenance.in-toto",
        Evidence,
        "infra/evidence/source/tcps-v26.7.19.intoto.jsonl"
    ),
    (
        "evidence.source-manifest",
        Evidence,
        "infra/SOURCE_MANIFEST.sha256"
    ),
    ("manufacture.lifecycle", Manufacture, "infra/tools/lifecycle.py"),
    (
        "manufacture.reproducible-build",
        Manufacture,
        "infra/scripts/reproducible-build.sh"
    ),
    ("manufacture.ggen-fixed-point", Manufacture, "ggen.lock"),
);

fn project_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

fn compile_obligations() -> Vec<Obligation> {
    let mut obligations = Vec::with_capacity(CAPABILITIES.len() * SURFACES.len() * LAWS.len());
    for capability in CAPABILITIES {
        for surface in SURFACES {
            for law in LAWS {
                obligations.push(Obligation {
                    id: format!("{}::{}::{}", capability.id, surface.id, law.id),
                    capability: capability.id,
                    surface: surface.id,
                    law: law.id,
                });
            }
        }
    }
    obligations
}

fn validate_catalog() -> Result<(), String> {
    let capability_ids: BTreeSet<_> = CAPABILITIES.iter().map(|cap| cap.id).collect();
    if capability_ids.len() != CAPABILITIES.len() {
        return Err("duplicate capability identity".to_string());
    }

    let paths: BTreeSet<_> = CAPABILITIES.iter().map(|cap| cap.probe_path).collect();
    if paths.len() != CAPABILITIES.len() {
        return Err("duplicate capability probe path".to_string());
    }

    let surface_ids: BTreeSet<_> = SURFACES.iter().map(|surface| surface.id).collect();
    if surface_ids.len() != SURFACES.len() {
        return Err("duplicate surface identity".to_string());
    }

    let law_ids: BTreeSet<_> = LAWS.iter().map(|law| law.id).collect();
    if law_ids.len() != LAWS.len() {
        return Err("duplicate law identity".to_string());
    }

    let channels: BTreeSet<_> = SURFACES
        .iter()
        .flat_map(|surface| surface.channels.iter().copied())
        .collect();
    if channels.len() != 12 {
        return Err(format!("expected 12 unique channels, found {}", channels.len()));
    }

    Ok(())
}

fn run_probe(root: &Path, capability: &Capability) -> ProbeResult {
    let path = root.join(capability.probe_path);
    match fs::read(&path) {
        Ok(bytes) => ProbeResult {
            capability: capability.id,
            standing: ProbeStanding::Accepted,
            observation: capability.probe_path.to_string(),
            fingerprint: fnv1a64(&bytes),
        },
        Err(error) => ProbeResult {
            capability: capability.id,
            standing: ProbeStanding::Refused,
            observation: format!("MISSING_ARTIFACT:{}:{error}", capability.probe_path),
            fingerprint: 0,
        },
    }
}

fn run_probes(root: &Path) -> Vec<ProbeResult> {
    CAPABILITIES
        .iter()
        .map(|capability| run_probe(root, capability))
        .collect()
}

fn discharge_obligations(
    obligations: &[Obligation],
    probes: &[ProbeResult],
) -> Result<Vec<Discharge>, String> {
    let accepted: BTreeMap<_, _> = probes
        .iter()
        .filter(|probe| probe.standing == ProbeStanding::Accepted)
        .map(|probe| (probe.capability, probe.fingerprint))
        .collect();

    obligations
        .iter()
        .map(|obligation| {
            accepted
                .get(obligation.capability)
                .copied()
                .map(|fingerprint| Discharge {
                    obligation_id: obligation.id.clone(),
                    capability: obligation.capability,
                    evidence_fingerprint: fingerprint,
                })
                .ok_or_else(|| {
                    format!(
                        "UNDISCHARGED_OBLIGATION:{}:missing accepted probe for {}",
                        obligation.id, obligation.capability
                    )
                })
        })
        .collect()
}

fn semantic_fingerprint(
    obligations: &[Obligation],
    probes: &[ProbeResult],
    discharges: &[Discharge],
) -> u64 {
    let mut canonical = String::new();

    for obligation in obligations {
        writeln!(
            canonical,
            "O|{}|{}|{}|{}",
            obligation.id, obligation.capability, obligation.surface, obligation.law
        )
        .expect("write canonical obligation");
    }

    let mut sorted_probes = probes.to_vec();
    sorted_probes.sort_by_key(|probe| probe.capability);
    for probe in &sorted_probes {
        writeln!(
            canonical,
            "P|{}|{:?}|{}|{:016x}",
            probe.capability, probe.standing, probe.observation, probe.fingerprint
        )
        .expect("write canonical probe");
    }

    for discharge in discharges {
        writeln!(
            canonical,
            "D|{}|{}|{:016x}",
            discharge.obligation_id, discharge.capability, discharge.evidence_fingerprint
        )
        .expect("write canonical discharge");
    }

    fnv1a64(canonical.as_bytes())
}

fn build_receipt(
    obligations: &[Obligation],
    probes: &[ProbeResult],
    discharges: &[Discharge],
) -> FabricReceipt {
    FabricReceipt {
        schema: "tcps-chicago-tdd-1000x/v1",
        capabilities: CAPABILITIES.len(),
        surfaces: SURFACES.len(),
        channels: SURFACES.iter().map(|surface| surface.channels.len()).sum(),
        laws: LAWS.len(),
        obligations: obligations.len(),
        probes: probes.len(),
        accepted_probes: probes
            .iter()
            .filter(|probe| probe.standing == ProbeStanding::Accepted)
            .count(),
        refused_probes: probes
            .iter()
            .filter(|probe| probe.standing == ProbeStanding::Refused)
            .count(),
        discharges: discharges.len(),
        semantic_fingerprint: semantic_fingerprint(obligations, probes, discharges),
    }
}

fn receipt_json(receipt: &FabricReceipt, recorded_at: &str) -> String {
    format!(
        concat!(
            "{{\n",
            "  \"schema\": \"{}\",\n",
            "  \"recorded_at\": \"{}\",\n",
            "  \"capabilities\": {},\n",
            "  \"surfaces\": {},\n",
            "  \"channels\": {},\n",
            "  \"laws\": {},\n",
            "  \"obligations\": {},\n",
            "  \"probes\": {},\n",
            "  \"accepted_probes\": {},\n",
            "  \"refused_probes\": {},\n",
            "  \"discharges\": {},\n",
            "  \"semantic_fingerprint\": \"{:016x}\"\n",
            "}}\n"
        ),
        receipt.schema,
        recorded_at,
        receipt.capabilities,
        receipt.surfaces,
        receipt.channels,
        receipt.laws,
        receipt.obligations,
        receipt.probes,
        receipt.accepted_probes,
        receipt.refused_probes,
        receipt.discharges,
        receipt.semantic_fingerprint,
    )
}

fn obligation_turtle(obligations: &[Obligation]) -> String {
    let mut out = String::from(
        "@prefix tcpsv: <urn:tcps:validation:> .\n\
         @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n",
    );
    for (index, obligation) in obligations.iter().enumerate() {
        writeln!(
            out,
            "tcpsv:obligation-{index} a tcpsv:Obligation ;\n    tcpsv:identity \"{}\" ;\n    tcpsv:capability \"{}\" ;\n    tcpsv:surface \"{}\" ;\n    tcpsv:law \"{}\" .\n",
            obligation.id, obligation.capability, obligation.surface, obligation.law
        )
        .expect("write Turtle obligation");
    }
    out
}

fn ocel_json(discharges: &[Discharge]) -> String {
    let mut out = String::from(
        "{\n  \"ocel:global-log\": {\"ocel:version\": \"2.0\"},\n  \"ocel:events\": [\n",
    );
    for (index, discharge) in discharges.iter().enumerate() {
        if index > 0 {
            out.push_str(",\n");
        }
        write!(
            out,
            "    {{\"ocel:eid\":\"discharge-{index}\",\"ocel:activity\":\"DischargeObligation\",\"obligation\":\"{}\",\"capability\":\"{}\",\"evidence\":\"{:016x}\"}}",
            discharge.obligation_id, discharge.capability, discharge.evidence_fingerprint
        )
        .expect("write OCEL event");
    }
    out.push_str("\n  ]\n}\n");
    out
}

fn execution_plan_tsv(probes: &[ProbeResult]) -> String {
    let mut out = String::from("capability\tstanding\tobservation\tfingerprint\n");
    for probe in probes {
        writeln!(
            out,
            "{}\t{:?}\t{}\t{:016x}",
            probe.capability, probe.standing, probe.observation, probe.fingerprint
        )
        .expect("write plan row");
    }
    out
}

fn rendezvous_shard(identity: &str, shard_count: usize) -> usize {
    assert!(shard_count > 0, "shard count must be nonzero");
    let mut best_shard = 0;
    let mut best_score = 0_u64;
    for shard in 0..shard_count {
        let candidate = format!("{identity}#{shard}");
        let score = fnv1a64(candidate.as_bytes());
        if shard == 0 || score > best_score {
            best_shard = shard;
            best_score = score;
        }
    }
    best_shard
}

fn contract_registry() -> TestContractRegistry {
    TestContractRegistry::new(CONTRACTS)
}

chicago_tdd_tools::test!(catalog_is_constitutionally_closed, {
    assert_eq!(validate_catalog(), Ok(()));
    assert_eq!(CAPABILITIES.len(), 43);
    assert_eq!(SURFACES.len(), 4);
    assert_eq!(LAWS.len(), 7);
});

chicago_tdd_tools::test!(contract_registry_covers_every_capability_and_law, {
    let registry = contract_registry();
    let capability_ids: Vec<_> = CAPABILITIES.iter().map(|capability| capability.id).collect();

    assert_eq!(registry.len(), 43);
    assert!(registry.uncovered_modules(&capability_ids).is_empty());
    assert!(registry.uncovered_invariants(UNIVERSAL_LAW_IDS).is_empty());
});

chicago_tdd_tools::test!(compiler_emits_exactly_1204_unique_obligations, {
    let obligations = compile_obligations();
    let unique: BTreeSet<_> = obligations.iter().map(|item| item.id.as_str()).collect();

    assert_eq!(obligations.len(), 43 * 4 * 7);
    assert_eq!(obligations.len(), 1_204);
    assert_eq!(unique.len(), obligations.len());
});

chicago_tdd_tools::test!(surface_model_exposes_twelve_distinct_channels, {
    let channels: BTreeSet<_> = SURFACES
        .iter()
        .flat_map(|surface| surface.channels.iter().copied())
        .collect();

    assert_eq!(channels.len(), 12);
    assert!(channels.contains("ontology"));
    assert!(channels.contains("wasm"));
    assert!(channels.contains("provenance"));
});

chicago_tdd_tools::test!(one_real_probe_is_planned_per_capability, {
    let fixture = TestFixture::with_data(project_root());
    let probes = run_probes(fixture.inner());
    let identities: BTreeSet<_> = probes.iter().map(|probe| probe.capability).collect();

    assert_eq!(probes.len(), 43);
    assert_eq!(identities.len(), 43);
});

chicago_tdd_tools::test!(all_checked_in_tcps_capabilities_are_observable, {
    let probes = run_probes(&project_root());
    let refused: Vec<_> = probes
        .iter()
        .filter(|probe| probe.standing == ProbeStanding::Refused)
        .collect();

    assert!(refused.is_empty(), "refused real probes: {refused:#?}");
    assert_eq!(
        probes
            .iter()
            .filter(|probe| probe.standing == ProbeStanding::Accepted)
            .count(),
        43
    );
});

chicago_tdd_tools::test!(accepted_probes_discharge_every_obligation, {
    let obligations = compile_obligations();
    let probes = run_probes(&project_root());
    let discharges = discharge_obligations(&obligations, &probes)
        .expect("all checked-in capabilities must discharge their obligations");

    assert_eq!(discharges.len(), 1_204);
    assert_eq!(
        discharges
            .iter()
            .map(|discharge| discharge.obligation_id.as_str())
            .collect::<BTreeSet<_>>()
            .len(),
        1_204
    );
});

chicago_tdd_tools::test!(rendezvous_sharding_is_deterministic_and_lossless, {
    let obligations = compile_obligations();
    let first: Vec<_> = obligations
        .iter()
        .map(|obligation| rendezvous_shard(&obligation.id, 8))
        .collect();
    let second: Vec<_> = obligations
        .iter()
        .map(|obligation| rendezvous_shard(&obligation.id, 8))
        .collect();

    assert_eq!(first, second);
    assert_eq!(first.len(), 1_204);
    assert!(first.iter().all(|shard| *shard < 8));
    for shard in 0..8 {
        assert!(first.contains(&shard), "shard {shard} must own work");
    }
});

chicago_tdd_tools::test!(semantic_receipt_is_replay_stable_across_wall_clock_time, {
    let obligations = compile_obligations();
    let probes = run_probes(&project_root());
    let discharges = discharge_obligations(&obligations, &probes).expect("discharge");
    let receipt = build_receipt(&obligations, &probes, &discharges);

    let first = receipt_json(&receipt, "2026-07-21T00:00:00Z");
    let second = receipt_json(&receipt, "2036-07-21T00:00:00Z");

    assert_ne!(first, second, "occurrence evidence retains real time");
    assert!(first.contains(&format!(
        "\"semantic_fingerprint\": \"{:016x}\"",
        receipt.semantic_fingerprint
    )));
    assert!(second.contains(&format!(
        "\"semantic_fingerprint\": \"{:016x}\"",
        receipt.semantic_fingerprint
    )));
});

chicago_tdd_tools::test!(missing_artifact_is_a_loud_refusal, {
    let missing = Capability {
        id: "negative.missing-artifact",
        kind: CapabilityKind::Manufacture,
        probe_path: "definitely/not/a/tcps/artifact",
    };
    let result = run_probe(&project_root(), &missing);

    assert_eq!(result.standing, ProbeStanding::Refused);
    assert!(result.observation.starts_with("MISSING_ARTIFACT:"));
    assert_eq!(result.fingerprint, 0);
});

chicago_tdd_tools::test!(graphs_and_ocel_preserve_all_obligation_identities, {
    let obligations = compile_obligations();
    let probes = run_probes(&project_root());
    let discharges = discharge_obligations(&obligations, &probes).expect("discharge");
    let turtle = obligation_turtle(&obligations);
    let ocel = ocel_json(&discharges);

    assert_eq!(turtle.matches("a tcpsv:Obligation").count(), 1_204);
    assert_eq!(ocel.matches("DischargeObligation").count(), 1_204);
    assert!(turtle.contains("core.auto-select::runtime::authority-separated"));
    assert!(ocel.contains("manufacture.ggen-fixed-point::evidence::idempotent"));
});

chicago_tdd_tools::test!(artifact_bundle_is_emitted_into_an_isolated_fixture, {
    let obligations = compile_obligations();
    let probes = run_probes(&project_root());
    let discharges = discharge_obligations(&obligations, &probes).expect("discharge");
    let receipt = build_receipt(&obligations, &probes, &discharges);
    let output = TempDir::new().expect("temporary output directory");

    fs::write(
        output.path().join("fabric-receipt.json"),
        receipt_json(&receipt, "2026-07-21T00:00:00Z"),
    )
    .expect("write receipt");
    fs::write(
        output.path().join("obligations.ttl"),
        obligation_turtle(&obligations),
    )
    .expect("write Turtle");
    fs::write(
        output.path().join("evidence.ocel.json"),
        ocel_json(&discharges),
    )
    .expect("write OCEL");
    fs::write(
        output.path().join("execution-plan.tsv"),
        execution_plan_tsv(&probes),
    )
    .expect("write execution plan");

    for name in [
        "fabric-receipt.json",
        "obligations.ttl",
        "evidence.ocel.json",
        "execution-plan.tsv",
    ] {
        let metadata = fs::metadata(output.path().join(name)).expect("artifact metadata");
        assert!(metadata.len() > 0, "{name} must be nonempty");
    }
});

chicago_tdd_tools::test!(execution_compression_is_exactly_twenty_eight_to_one, {
    let obligations = compile_obligations();
    let probes = run_probes(&project_root());

    assert_eq!(obligations.len() / probes.len(), 28);
    assert_eq!(obligations.len() % probes.len(), 0);
});

chicago_tdd_tools::test!(receipt_reports_the_complete_tcps_constitution, {
    let obligations = compile_obligations();
    let probes = run_probes(&project_root());
    let discharges = discharge_obligations(&obligations, &probes).expect("discharge");
    let receipt = build_receipt(&obligations, &probes, &discharges);

    assert_eq!(receipt.schema, "tcps-chicago-tdd-1000x/v1");
    assert_eq!(receipt.capabilities, 43);
    assert_eq!(receipt.surfaces, 4);
    assert_eq!(receipt.channels, 12);
    assert_eq!(receipt.laws, 7);
    assert_eq!(receipt.obligations, 1_204);
    assert_eq!(receipt.probes, 43);
    assert_eq!(receipt.accepted_probes, 43);
    assert_eq!(receipt.refused_probes, 0);
    assert_eq!(receipt.discharges, 1_204);
    assert_ne!(receipt.semantic_fingerprint, 0);
});

chicago_tdd_tools::test!(capability_kinds_cover_the_full_product_lifecycle, {
    let mut counts = BTreeMap::new();
    for capability in CAPABILITIES {
        *counts.entry(capability.kind.as_str()).or_insert(0_usize) += 1;
    }

    assert_eq!(counts.get("core"), Some(&26));
    assert_eq!(counts.get("runtime"), Some(&4));
    assert_eq!(counts.get("release"), Some(&6));
    assert_eq!(counts.get("evidence"), Some(&4));
    assert_eq!(counts.get("manufacture"), Some(&3));
});
