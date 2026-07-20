# 豊田コード生産方式 製品版 v26.7.19

原初の豊田生産方式を、日本語の歴史語彙・生産語彙・改善語彙を正準の意味体系としてRustへ写像した製品用ワークスペースです。英語の一般的なソフトウェア語彙へいったん翻訳してから実装するのではなく、`自働化`、`後工程引取り`、`標準作業`、`異常停止`、`真因`、`改善`を型・状態遷移・数量保存則・受領証として直接実装します。

> このプロジェクトは独立した研究・実装であり、Toyota Motor CorporationまたはToyota Production Systemの公式成果物ではありません。

## 固定処理系

全工程はRust 1.97.1へ固定する。1.97.0で報告されたLLVM誤コンパイルを修正した点版であり、製品構築は1.97.0へ暗黙に後退してはならない。

## 製品境界

```text
日本語TPS意味体系
    ↓
tcps-core          no_std・外部依存なし・全組込み対象の基準
    ├── tcps-std   環境観測・受領証永続化
    ├── tcps-ffi   C ABI・静的/動的ライブラリ
    ├── tcps-wasm  WebAssembly能力境界
    └── tcps-cli   現地検査・基準計測
```

中核則は変わりません。

```text
原問題 → 発明 → 不変条件 → 標準作業 → 実行 → 異常 → 停止 → 真因 → 対策 → 標準更新
選択 ≠ 許可 ≠ 実行
```

## 対象対応

Rustが組込み対象として認識する全三つ組は、実行時に次の命令から取得します。

```bash
rustc --print target-list
```

製品ライフサイクルは対象を三階層へ分類します。

- 第一階層: Rustプロジェクトが構築と試験を保証する対象
- 第二階層: Rustプロジェクトが構築を保証する対象。試験は対象ごとの実行環境が必要
- 第三階層: コンパイラ内に存在するが保証のない対象。`nightly -Z build-std=core`を用いた最善努力

`tcps-core`はすべての対象で構築を試みます。CLI、C ABI、WASM、Apple、Androidなどの製品は、その対象で意味のあるSDKと実行環境がある場合だけ追加します。SDK不足、実行環境不足、対象未配布、コンパイラ拒否は別々の結果として受領証へ記録されます。

## 最短経路

### Unix系

```bash
./scripts/bootstrap.sh
./scripts/full-lifecycle.sh
```

### Windows PowerShell

```powershell
./scripts/bootstrap.ps1
./scripts/full-lifecycle.ps1
```

### 全組込み対象の走査

```bash
./scripts/full-lifecycle-all-targets.sh
```

```powershell
./scripts/full-lifecycle-all-targets.ps1
```

全対象で意味が共通する製品は`no_std`中核である。標準ライブラリ、C ABI、CLI、WASM、Apple、Androidなどの配布物は、対象能力とSDKが成立する工程だけで引き取る。

## 個別工程

```bash
python3 tools/lifecycle.py doctor
python3 tools/lifecycle.py verify
python3 tools/lifecycle.py matrix
python3 tools/lifecycle.py build --target x86_64-unknown-linux-gnu --product core
python3 tools/lifecycle.py test --target x86_64-unknown-linux-gnu
python3 tools/lifecycle.py benchmark --iterations 1000000
python3 tools/lifecycle.py package --target x86_64-unknown-linux-gnu
python3 tools/lifecycle.py sbom
python3 tools/lifecycle.py provenance
python3 tools/lifecycle.py checksums
python3 tools/lifecycle.py sign --method cosign
python3 tools/lifecycle.py verify-release
```

## 全対象構築

第一・第二階層の中核を構築します。

```bash
python3 tools/lifecycle.py build-all --tiers 1,2 --product core
```

第三階層を16分割して最善努力で構築します。

```bash
python3 tools/lifecycle.py build-all   --tiers 3   --product core   --shard-index 0   --shard-count 16
```

第三階層では、対象の`core`がrustup配布に存在しない場合、導入済みnightlyと`rust-src`を用いて`-Z build-std=core`を試します。結果を成功に見せかけることはありません。

## プラットフォーム別経路

| 系統 | スクリプト | 必要な外部能力 |
|---|---|---|
| Linux GNU/musl | `scripts/platform/linux.sh` | 対象ごとのリンカとsysroot |
| Apple | `scripts/platform/apple.sh` | macOSとXcode SDK |
| Android | `scripts/platform/android.py` | Android NDK |
| Windows | `scripts/platform/windows.ps1` | MSVCまたはMinGW/LLVM-mingw |
| WebAssembly | `scripts/platform/wasm.sh` | 対象に応じてWASI SDK/Emscripten/実行器 |
| 組込み | `scripts/platform/embedded.sh` | 対象リンカ、実機またはエミュレータ |
| BSD/Solaris | `scripts/platform/bsd-solaris.sh` | 対象sysrootまたは対象ホスト |
| UEFI | `scripts/platform/uefi.sh` | UEFI画像作成器とQEMU/実機 |
| GPU/eBPF | `scripts/platform/gpu.sh` | CUDA/ROCm/eBPFツールチェーン |

## 受領証と供給網

各工程は`receipts/`へJSON受領証を出します。公開物には次を生成します。

- 対象行列
- 対象別ZIPとtar.gz
- SHA-256一覧
- CycloneDX 1.5 SBOM
- SPDX 2.3 SBOM
- in-toto Statement / SLSA provenance
- 任意のGPG、minisign、cosign署名

中核には依存なしのSHA-256実装があり、製品方策の要約へ使用します。旧`簡易要約`は互換用の非暗号学的要約であり、製品の信頼境界には使用しません。

## CI

同梱経路:

- GitHub Actions: ホスト検査、第一階層、代表第二階層、全組込み対象分割、供給網、公開
- GitLab CI
- Azure Pipelines

第一階層は失敗を許容しません。第二階層は構築失敗を拒否します。第三階層は既定では最善努力ですが、`--strict-tier3`で拒否条件にできます。

## ワークスペース

```text
crates/
├── tcps-core   日本語TPS中核、no_std
├── tcps-std    標準環境統合
├── tcps-ffi    C ABI
├── tcps-wasm   WebAssembly公開面
└── tcps-cli    現地検査と基準計測

tools/lifecycle.py       全環境共通ライフサイクル
scripts/                 Bash、PowerShell、プラットフォーム別経路
targets/                 公式階層スナップショットと対象系統
schemas/                 受領証・対象行列・公開記録のJSON Schema
packaging/               Debian、RPM、npm、NuGet、Apple、Android素材
.github/workflows/       継続的検査と公開
docs/                    運用・安全・移植・ABI文書
```

## 立脚点

この生成環境にはRustツールチェーンがなく、外部ネットワークからrustupを取得できませんでした。そのため、この配布物は構造検査、Python実行器検査、対象分類検査、アーカイブ検査まで閉じています。Rustのコンパイル、Clippy、全試験、ABI生成、対象別構築の立脚点は、同梱CIまたは承認済みRust 1.97.1環境で受領証を生成した時点で成立します。

## ライセンス

MIT OR Apache-2.0
