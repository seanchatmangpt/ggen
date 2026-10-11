$ErrorActionPreference = "Stop"
$Root = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
$Python = if (Get-Command python3 -ErrorAction SilentlyContinue) { "python3" } else { "python" }
& $Python (Join-Path $Root "tools/lifecycle.py") @args
exit $LASTEXITCODE
