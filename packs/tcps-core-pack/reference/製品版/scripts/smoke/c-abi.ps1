$ErrorActionPreference = "Stop"
$Root = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
Set-Location $Root
New-Item -ItemType Directory -Force .lifecycle | Out-Null
$HostTarget = (rustc -vV | Select-String '^host: ').Line.Replace('host: ', '')
cargo build -p tcps-ffi --release --target $HostTarget
$Out = Join-Path $Root "target/$HostTarget/release"
$Compiler = Get-Command cl.exe -ErrorAction SilentlyContinue
if (-not $Compiler) { throw "cl.exe is required" }
& cl.exe /nologo /I include tests/c/abi_smoke.c /link "/LIBPATH:$Out" tcps_ffi.lib "/OUT:.lifecycle/c-abi-smoke.exe"
& ./.lifecycle/c-abi-smoke.exe
