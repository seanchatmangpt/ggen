$ErrorActionPreference = "Continue"
$Root = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$Lifecycle = Join-Path $Root "scripts/lifecycle.ps1"
$Targets = @(
  "x86_64-pc-windows-msvc", "i686-pc-windows-msvc", "aarch64-pc-windows-msvc",
  "x86_64-pc-windows-gnu", "i686-pc-windows-gnu",
  "x86_64-pc-windows-gnullvm", "aarch64-pc-windows-gnullvm"
)
foreach ($Target in $Targets) {
    & $Lifecycle build --target $Target --product core
    & $Lifecycle build --target $Target --product ffi
}
