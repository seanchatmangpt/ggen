# 既存状態観測製造票

## 目的

この製造票は、既存リポジトリを変更する前に、現実に存在する枝、検査、公開路、権限、秘密要求、生成面、手動作業、外部利用先を観測し、`as-found`状態として固定します。

## 基準法則

- `as-found`は`as-desired`より先に造る。
- GitHub画面、設定ファイル、担当者報告のいずれも単独ではstandingを持たない。
- 不明を空欄または緑へ丸めない。
- 本製造票は読取り専用であり、外部状態を変更しない。

## 主な生成品

- `docs/retrofit/as-found/現況調査票.md`
- `docs/retrofit/as-found/未知条件台帳.md`
- `docs/retrofit/as-found/現況構成.mmd`
- `.tcps/as-found-observation.json`
