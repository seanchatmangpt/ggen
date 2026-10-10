$ErrorActionPreference = "Stop"
$Lifecycle = Join-Path $PSScriptRoot "lifecycle.ps1"
& $Lifecycle doctor
& $Lifecycle verify
& $Lifecycle matrix --output targets/target-matrix-active.json
& $Lifecycle build-all --tiers 1,2 --product core
& $Lifecycle build-all --tiers 3 --product core
$HostTarget = (rustc -vV | Select-String '^host: ').Line.Replace('host: ', '')
& $Lifecycle test --target $HostTarget
$Iterations = if ($env:TCPS_BENCH_ITERATIONS) { $env:TCPS_BENCH_ITERATIONS } else { "1000000" }
& $Lifecycle benchmark --iterations $Iterations
foreach ($Product in @('std', 'ffi', 'cli')) {
    & $Lifecycle build --target $HostTarget --product $Product
}
cargo run -p tcps-cli --bin tcps -- 検査
& (Join-Path $PSScriptRoot "smoke/c-abi.ps1")
& $Lifecycle package --target $HostTarget
& $Lifecycle sbom
& $Lifecycle provenance
& $Lifecycle checksums
& $Lifecycle verify-release
