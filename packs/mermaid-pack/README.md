# ggen Mermaid製造票 第二六・七・二〇版

## 目的

この製造票は、Mermaid図面を受入済みRDFから生成し、図種ごとの上流描画器出典を
commit固定で記録し、構文検査、SVG描画、図面台帳、standing上限へ接続します。

## 上流根拠

- Mermaid版: `11.16.0`
- 上流commit: `f0ffb41c1ee1ff667b528e86c3b082249726eeef`
- 上流許諾: MIT
- 中央登録: `packages/mermaid/src/diagram-api/diagram-orchestration.ts`
- 登録関数: `packages/mermaid/src/diagram-api/diagramAPI.ts`
- 実行時解決: `packages/mermaid/src/Diagram.ts`
- 描画契約: `packages/mermaid/src/diagram-api/types.ts`

## 権威区分

1. 規範図面はRDFからggenが生成します。
2. コード走査またはAIが生成した図面は観測です。
3. 観測図面は、規範図面を自動的に上書きできません。
4. Mermaid Diagram Sync等の外部アプリは観測補助としてのみ使用します。

## 収録物

- `ontology.ttl`: 39個の上流図種・描画器出典と三つの規範図面
- `gates/`: 出典固定、未知図種、standing上方丸めを拒否
- `templates/diagram.mmd.tmpl`: RDFのMermaid原文を図面へ投影
- `templates/renderer_source_catalog.md.tmpl`: 描画器出典台帳
- `scripts/resolve-renderers.mjs`: 上流登録路を動的に追跡
- `templates/verify_mermaid.mjs.tmpl`: 全`.mmd`の構文検査
- `templates/render_mermaid.sh.tmpl`: Mermaid CLIによるSVG生成

## 使用順序

```bash
ggen sync
npm install
npm run mermaid:verify
npm run mermaid:render
node packs/mermaid-pack/scripts/resolve-renderers.mjs
```

## 完成判定

- 二回同期で`.mmd`がbyte-identical
- 全図面が`mermaid.parse`を通過
- 全図面が`mmdc`でSVGへ描画可能
- 描画器出典が上流commitへ固定
- 規範図面への手修正が漂流として拒否
- 上流版変更時に出典台帳が再製造される
