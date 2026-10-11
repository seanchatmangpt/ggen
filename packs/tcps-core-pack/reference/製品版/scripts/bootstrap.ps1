$ErrorActionPreference = "Stop"
$Toolchain = if ($env:TCPS_RUST_TOOLCHAIN) { $env:TCPS_RUST_TOOLCHAIN } else { "1.97.1" }
if (-not (Get-Command rustup -ErrorAction SilentlyContinue)) {
    throw "rustup is not installed. Use the organization-approved rustup installation procedure from https://rustup.rs."
}
rustup toolchain install $Toolchain --profile minimal --component rustfmt --component clippy --component rust-src
rustup override set $Toolchain
if ($env:TCPS_INSTALL_NIGHTLY -eq "1") {
    rustup toolchain install nightly --profile minimal --component rust-src
}
& (Join-Path $PSScriptRoot "lifecycle.ps1") doctor
