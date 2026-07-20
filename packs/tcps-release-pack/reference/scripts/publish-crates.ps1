$ErrorActionPreference = "Stop"
$Root = Split-Path -Parent $PSScriptRoot
Set-Location $Root
python tools/lifecycle.py verify
$Packages = @("tcps-core", "tcps-std", "tcps-ffi", "tcps-wasm", "tcps-cli")
foreach ($Package in $Packages) {
    Write-Host "公開検査: $Package"
    if ($env:TCPS_PUBLISH -eq "1") {
        cargo publish -p $Package
        $Wait = if ($env:TCPS_PUBLISH_WAIT_SECONDS) { [int]$env:TCPS_PUBLISH_WAIT_SECONDS } else { 30 }
        Start-Sleep -Seconds $Wait
    } else {
        cargo publish -p $Package --dry-run
    }
}
