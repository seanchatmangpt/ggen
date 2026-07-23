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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "kebab-case")]
enum CapabilityKind {
    Core,
    Runtime,
    Release,
    Evidence,
    Manufacture,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
enum ReferenceStanding {
    ExactManifest,
    DeclaredDerivative,
    Independent,
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
    reference_standing: ReferenceStanding,
    validator: Validator,
}

macro_rules! capabilities {
    ($(($id:literal, $kind:ident, $path:expr, $manifest:expr, $standing:ident, $validator:ident)),+ $(,)?) => {
        const CAPABILITIES: &[Capability] = &[
            $(Capability {
                id: $id,
                kind: CapabilityKind::$kind,
                path: $path,
                manifest_path: $manifest,
                reference_standing: ReferenceStanding::$standing,
                validator: Validator::$validator,
            },)+
        ];
    };
}

capabilities!(
    ("core.vocabulary", Core, "src/語彙.rs", Some("crates/tcps-core/src/語彙.rs"), ExactManifest, RustLibrary),
    ("core.origin", Core, "src/原点.rs", Some("crates/tcps-core/src/原点.rs"), ExactManifest, RustLibrary),
    ("core.lineage", Core, "src/系譜.rs", Some("crates/tcps-core/src/系譜.rs"), ExactManifest, RustLibrary),
    ("core.quality", Core, "src/品質.rs", Some("crates/tcps-core/src/品質.rs"), ExactManifest, RustLibrary),
    ("core.standard-work", Core, "src/標準作業.rs", Some("crates/tcps-core/src/標準作業.rs"), ExactManifest, RustLibrary),
    ("core.jidoka", Core, "src/自働化.rs", Some("crates/tcps-core/src/自働化.rs"), ExactManifest, RustLibrary),
    ("core.kanban", Core, "src/かんばん.rs", Some("crates/tcps-core/src/かんばん.rs"), ExactManifest, RustLibrary),
    ("core.just-in-time", Core, "src/必要時生産.rs", Some("crates/tcps-core/src/必要時生産.rs"), ExactManifest, RustLibrary),
    ("core.heijunka", Core, "src/平準化.rs", Some("crates/tcps-core/src/平準化.rs"), ExactManifest, RustLibrary),
    ("core.andon", Core, "src/アンドン.rs", Some("crates/tcps-core/src/アンドン.rs"), ExactManifest, RustLibrary),
    ("core.kaizen", Core, "src/改善.rs", Some("crates/tcps-core/src/改善.rs"), DeclaredDerivative, RustLibrary),
    ("core.receipt", Core, "src/受領証.rs", Some("crates/tcps-core/src/受領証.rs"), ExactManifest, RustLibrary),
    ("core.crypto-digest", Core, "src/暗号要約.rs", Some("crates/tcps-core/src/暗号要約.rs"), DeclaredDerivative, RustLibrary),
    ("core.auto-select", Core, "src/自動選択.rs", Some("crates/tcps-core/src/自動選択.rs"), DeclaredDerivative, RustLibrary),
    ("core.blue-river-dam", Core, "src/青い川のダム.rs", Some("crates/tcps-core/src/青い川のダム.rs"), DeclaredDerivative, RustLibrary),
    ("core.whole-system", Core, "src/全体.rs", Some("crates/tcps-core/src/全体.rs"), ExactManifest, RustLibrary),
    ("core.canonical", Core, "src/正準.rs", Some("crates/tcps-core/src/正準.rs"), ExactManifest, RustLibrary),
    ("core.genchi-genbutsu", Core, "src/現地現物.rs", Some("crates/tcps-core/src/現地現物.rs"), ExactManifest, RustLibrary),
    ("core.poka-yoke", Core, "src/ポカヨケ.rs", Some("crates/tcps-core/src/ポカヨケ.rs"), ExactManifest, RustLibrary),
    ("core.five-whys", Core, "src/五回なぜ.rs", Some("crates/tcps-core/src/五回なぜ.rs"), ExactManifest, RustLibrary),
    ("core.human-centered", Core, "src/人間中心.rs", Some("crates/tcps-core/src/人間中心.rs"), ExactManifest, RustLibrary),
    ("core.muri-mura-muda", Core, "src/ムリムラムダ.rs", Some("crates/tcps-core/src/ムリムラムダ.rs"), ExactManifest, RustLibrary),
    ("core.safety", Core, "src/安全.rs", Some("crates/tcps-core/src/安全.rs"), ExactManifest, RustLibrary),
    ("core.takt", Core, "src/タクト.rs", Some("crates/tcps-core/src/タクト.rs"), ExactManifest, RustLibrary),
    ("core.value-stream", Core, "src/価値流.rs", Some("crates/tcps-core/src/価値流.rs"), ExactManifest, RustLibrary),
    ("core.process-capability", Core, "src/工程能力.rs", Some("crates/tcps-core/src/工程能力.rs"), ExactManifest, RustLibrary),
    ("runtime.std", Runtime, "crates/tcps-std/src/lib.rs", Some("crates/tcps-std/src/lib.rs"), DeclaredDerivative, RustLibrary),
    ("runtime.ffi", Runtime, "crates/tcps-ffi/src/lib.rs", Some("crates/tcps-ffi/src/lib.rs"), ExactManifest, RustLibrary),
    ("runtime.wasm", Runtime, "crates/tcps-wasm/src/lib.rs", Some("crates/tcps-wasm/src/lib.rs"), ExactManifest, RustLibrary),
    ("runtime.cli", Runtime, "crates/tcps-cli/src/main.rs", Some("crates/tcps-cli/src/main.rs"), ExactManifest, RustBinary),
    ("release.ci", Release, "infra/ci/tier1.yml", Some(".github/workflows/tier1.yml"), ExactManifest, WorkflowYaml),
    ("release.targets", Release, "infra/targets/tier1.txt", Some("targets/tier1.txt"), ExactManifest, TargetList),
    ("release.packaging.deb", Release, "infra/packaging/deb/control.in", Some("packaging/deb/control.in"), ExactManifest, DebianControl),
    ("release.packaging.rpm", Release, "infra/packaging/rpm/tcps.spec.in", Some("packaging/rpm/tcps.spec.in"), ExactManifest, RpmSpec),
    ("release.packaging.npm", Release, "infra/packaging/npm/package.json.in", Some("packaging/npm/package.json.in"), ExactManifest, NpmPackage),
    ("release.packaging.nuget", Release, "infra/packaging/nuget/tcps.nuspec.in", Some("packaging/nuget/tcps.nuspec.in"), ExactManifest, NugetPackage),
    ("evidence.sbom.cyclonedx", Evidence, "infra/evidence/source/tcps-v26.7.19.cdx.json", Some("evidence/source/tcps-v26.7.19.cdx.json"), ExactManifest, CycloneDx),
    ("evidence.sbom.spdx", Evidence, "infra/evidence/source/tcps-v26.7.19.spdx.json", Some("evidence/source/tcps-v26.7.19.spdx.json"), ExactManifest, Spdx),
    ("evidence.provenance.in-toto", Evidence, "infra/evidence/source/tcps-v26.7.19.intoto.jsonl", Some("evidence/source/tcps-v26.7.19.intoto.jsonl"), ExactManifest, InTotoJsonLines),
    ("evidence.source-manifest", Evidence, MANIFEST_PATH, None, Independent, SourceManifest),
    ("manufacture.lifecycle", Manufacture, "infra/tools/lifecycle.py", Some("tools/lifecycle.py"), ExactManifest, PythonSyntax),
    ("manufacture.reproducible-build", Manufacture, "infra/scripts/reproducible-build.sh", Some("scripts/reproducible-build.sh"), ExactManifest, ShellSyntax),
    ("manufacture.ggen-fixed-point", Manufacture, "ggen.lock", None, Independent, GgenLock),
);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct CapabilityEvidence {
    id: &'static str,
    kind: CapabilityKind,
    path: &'static str,
    byte_len: usize,
    sha256: String,
    reference_standing: ReferenceStanding,
    reference_sha256: Option<String>,
    facts: Vec<&'static str>,
    evidence_digest: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct EvidenceReceipt {
    schema: &'static str,
    capability_count: usize,
    manifest_entry_count: usize,
    exact_manifest_capabilities: usize,
    declared_derivative_capabilities: usize,
    independent_capabilities: usize,
    pack_count: usize,
    evidence_root: String,
}
