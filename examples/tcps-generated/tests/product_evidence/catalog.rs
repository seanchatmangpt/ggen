const MANIFEST_PATH: &str = "infra/SOURCE_MANIFEST.sha256";
const EXPECTED_MANIFEST_ENTRIES: usize = 129;
const EXPECTED_PACKS: [&str; 8] = [
    "ggen-verify-pack",
    "tcps-cli-pack",
    "tcps-core-pack",
    "tcps-evidence",
    "tcps-ffi-pack",
    "tcps-release-pack",
    "tcps-std-pack",
    "tcps-wasm-pack",
];

const CONTRACTS: &[TestContract] = &[
    TestContract::new(
        "tcps-evidence/source-manifest",
        &["tcps.source-manifest"],
        &["unique-paths", "sha256-format", "exact-entry-count"],
        ResourceEnvelope::new(u64::MAX, 64 * 1024 * 1024, false, true, false),
        &["real-filesystem"],
    ),
    TestContract::new(
        "tcps-evidence/capabilities",
        &["tcps.capability-catalog"],
        &["format-parse", "manifest-bound", "no-vacuous-discharge"],
        ResourceEnvelope::new(u64::MAX, 64 * 1024 * 1024, false, true, false),
        &["real-filesystem", "python3", "bash"],
    ),
    TestContract::new(
        "tcps-evidence/ggen-lock",
        &["tcps.ggen-lock"],
        &["complete-pack-set", "blake3-identity-format"],
        ResourceEnvelope::new(u64::MAX, 8 * 1024 * 1024, false, true, false),
        &["real-filesystem"],
    ),
    TestContract::new(
        "tcps-evidence/receipt",
        &["tcps.evidence-receipt"],
        &["cryptographic-root", "replay-stable", "mutation-sensitive"],
        ResourceEnvelope::new(u64::MAX, 64 * 1024 * 1024, false, true, false),
        &["real-filesystem"],
    ),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "kebab-case")]
enum CapabilityKind {
    Core,
    Runtime,
    Release,
    Evidence,
    Manufacture,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Validator {
    RustLibrary,
    RustBinary,
    WorkflowYaml,
    TargetList,
    DebianControl,
    RpmSpec,
    NpmPackage,
    NugetPackage,
    CycloneDx,
    Spdx,
    InTotoJsonLines,
    SourceManifest,
    PythonSyntax,
    ShellSyntax,
    GgenLock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Capability {
    id: &'static str,
    kind: CapabilityKind,
    path: &'static str,
    manifest_path: Option<&'static str>,
    validator: Validator,
}

macro_rules! capabilities {
    ($(($id:literal, $kind:ident, $path:expr, $manifest:expr, $validator:ident)),+ $(,)?) => {
        const CAPABILITIES: &[Capability] = &[
            $(Capability {
                id: $id,
                kind: CapabilityKind::$kind,
                path: $path,
                manifest_path: $manifest,
                validator: Validator::$validator,
            },)+
        ];
    };
}

capabilities!(
    ("core.vocabulary", Core, "src/語彙.rs", Some("crates/tcps-core/src/語彙.rs"), RustLibrary),
    ("core.origin", Core, "src/原点.rs", Some("crates/tcps-core/src/原点.rs"), RustLibrary),
    ("core.lineage", Core, "src/系譜.rs", Some("crates/tcps-core/src/系譜.rs"), RustLibrary),
    ("core.quality", Core, "src/品質.rs", Some("crates/tcps-core/src/品質.rs"), RustLibrary),
    ("core.standard-work", Core, "src/標準作業.rs", Some("crates/tcps-core/src/標準作業.rs"), RustLibrary),
    ("core.jidoka", Core, "src/自働化.rs", Some("crates/tcps-core/src/自働化.rs"), RustLibrary),
    ("core.kanban", Core, "src/かんばん.rs", Some("crates/tcps-core/src/かんばん.rs"), RustLibrary),
    ("core.just-in-time", Core, "src/必要時生産.rs", Some("crates/tcps-core/src/必要時生産.rs"), RustLibrary),
    ("core.heijunka", Core, "src/平準化.rs", Some("crates/tcps-core/src/平準化.rs"), RustLibrary),
    ("core.andon", Core, "src/アンドン.rs", Some("crates/tcps-core/src/アンドン.rs"), RustLibrary),
    ("core.kaizen", Core, "src/改善.rs", Some("crates/tcps-core/src/改善.rs"), RustLibrary),
    ("core.receipt", Core, "src/受領証.rs", Some("crates/tcps-core/src/受領証.rs"), RustLibrary),
    ("core.crypto-digest", Core, "src/暗号要約.rs", Some("crates/tcps-core/src/暗号要約.rs"), RustLibrary),
    ("core.auto-select", Core, "src/自動選択.rs", Some("crates/tcps-core/src/自動選択.rs"), RustLibrary),
    ("core.blue-river-dam", Core, "src/青い川のダム.rs", Some("crates/tcps-core/src/青い川のダム.rs"), RustLibrary),
    ("core.whole-system", Core, "src/全体.rs", Some("crates/tcps-core/src/全体.rs"), RustLibrary),
    ("core.canonical", Core, "src/正準.rs", Some("crates/tcps-core/src/正準.rs"), RustLibrary),
    ("core.genchi-genbutsu", Core, "src/現地現物.rs", Some("crates/tcps-core/src/現地現物.rs"), RustLibrary),
    ("core.poka-yoke", Core, "src/ポカヨケ.rs", Some("crates/tcps-core/src/ポカヨケ.rs"), RustLibrary),
    ("core.five-whys", Core, "src/五回なぜ.rs", Some("crates/tcps-core/src/五回なぜ.rs"), RustLibrary),
    ("core.human-centered", Core, "src/人間中心.rs", Some("crates/tcps-core/src/人間中心.rs"), RustLibrary),
    ("core.muri-mura-muda", Core, "src/ムリムラムダ.rs", Some("crates/tcps-core/src/ムリムラムダ.rs"), RustLibrary),
    ("core.safety", Core, "src/安全.rs", Some("crates/tcps-core/src/安全.rs"), RustLibrary),
    ("core.takt", Core, "src/タクト.rs", Some("crates/tcps-core/src/タクト.rs"), RustLibrary),
    ("core.value-stream", Core, "src/価値流.rs", Some("crates/tcps-core/src/価値流.rs"), RustLibrary),
    ("core.process-capability", Core, "src/工程能力.rs", Some("crates/tcps-core/src/工程能力.rs"), RustLibrary),
    ("runtime.std", Runtime, "crates/tcps-std/src/lib.rs", Some("crates/tcps-std/src/lib.rs"), RustLibrary),
    ("runtime.ffi", Runtime, "crates/tcps-ffi/src/lib.rs", Some("crates/tcps-ffi/src/lib.rs"), RustLibrary),
    ("runtime.wasm", Runtime, "crates/tcps-wasm/src/lib.rs", Some("crates/tcps-wasm/src/lib.rs"), RustLibrary),
    ("runtime.cli", Runtime, "crates/tcps-cli/src/main.rs", Some("crates/tcps-cli/src/main.rs"), RustBinary),
    ("release.ci", Release, "infra/ci/tier1.yml", Some(".github/workflows/tier1.yml"), WorkflowYaml),
    ("release.targets", Release, "infra/targets/tier1.txt", Some("targets/tier1.txt"), TargetList),
    ("release.packaging.deb", Release, "infra/packaging/deb/control.in", Some("packaging/deb/control.in"), DebianControl),
    ("release.packaging.rpm", Release, "infra/packaging/rpm/tcps.spec.in", Some("packaging/rpm/tcps.spec.in"), RpmSpec),
    ("release.packaging.npm", Release, "infra/packaging/npm/package.json.in", Some("packaging/npm/package.json.in"), NpmPackage),
    ("release.packaging.nuget", Release, "infra/packaging/nuget/tcps.nuspec.in", Some("packaging/nuget/tcps.nuspec.in"), NugetPackage),
    ("evidence.sbom.cyclonedx", Evidence, "infra/evidence/source/tcps-v26.7.19.cdx.json", Some("evidence/source/tcps-v26.7.19.cdx.json"), CycloneDx),
    ("evidence.sbom.spdx", Evidence, "infra/evidence/source/tcps-v26.7.19.spdx.json", Some("evidence/source/tcps-v26.7.19.spdx.json"), Spdx),
    ("evidence.provenance.in-toto", Evidence, "infra/evidence/source/tcps-v26.7.19.intoto.jsonl", Some("evidence/source/tcps-v26.7.19.intoto.jsonl"), InTotoJsonLines),
    ("evidence.source-manifest", Evidence, MANIFEST_PATH, None, SourceManifest),
    ("manufacture.lifecycle", Manufacture, "infra/tools/lifecycle.py", Some("tools/lifecycle.py"), PythonSyntax),
    ("manufacture.reproducible-build", Manufacture, "infra/scripts/reproducible-build.sh", Some("scripts/reproducible-build.sh"), ShellSyntax),
    ("manufacture.ggen-fixed-point", Manufacture, "ggen.lock", None, GgenLock),
);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct CapabilityEvidence {
    id: &'static str,
    kind: CapabilityKind,
    path: &'static str,
    byte_len: usize,
    sha256: String,
    manifest_bound: bool,
    facts: Vec<&'static str>,
    evidence_digest: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct EvidenceReceipt {
    schema: &'static str,
    capability_count: usize,
    manifest_entry_count: usize,
    manifest_bound_capabilities: usize,
    pack_count: usize,
    evidence_root: String,
}
