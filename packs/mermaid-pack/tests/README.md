# 検査標準

1. `010_upstream_pin.rq`は上流版、commit、許諾の欠落を拒否します。
2. `020_renderer_source_required.rq`は図種の出典欠落を拒否します。
3. `040_no_unknown_type.rq`は未登録図種を拒否します。
4. `050_normative_must_be_generated.rq`は手書き規範図面を拒否します。
5. 生成後は`npm run mermaid:verify`と`npm run mermaid:render`を実行します。
6. 二回目の`ggen sync`で差分がないことを固定点証拠とします。
