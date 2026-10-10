$ErrorActionPreference = "Stop"
& (Join-Path $PSScriptRoot "lifecycle.ps1") doctor
& (Join-Path $PSScriptRoot "lifecycle.ps1") verify
& (Join-Path $PSScriptRoot "lifecycle.ps1") matrix
$HostTarget = (rustc -vV | Select-String '^host: ').Line.Replace('host: ', '')
foreach ($Product in @('core', 'std', 'ffi', 'cli')) {
    & (Join-Path $PSScriptRoot "lifecycle.ps1") build --target $HostTarget --product $Product
}
cargo run -p tcps-cli --bin tcps -- 検査
& (Join-Path $PSScriptRoot "smoke/c-abi.ps1")
& (Join-Path $PSScriptRoot "lifecycle.ps1") package --target $HostTarget
& (Join-Path $PSScriptRoot "lifecycle.ps1") sbom
& (Join-Path $PSScriptRoot "lifecycle.ps1") provenance
& (Join-Path $PSScriptRoot "lifecycle.ps1") checksums
& (Join-Path $PSScriptRoot "lifecycle.ps1") verify-release
