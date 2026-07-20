from __future__ import annotations

import re
import sys
from pathlib import Path

根 = Path(__file__).resolve().parents[1]

許可語 = {
    'Clone','Copy','Debug','Eq','PartialEq','PhantomData','Self','as','assert','assert_eq',
    'bool','cfg','const','core','crate','deny','derive','else','enum','extern','false','fn',
    'for','forbid','if','impl','in','len','let','marker','match','matches',
    'missing_debug_implementations','mod','mut','no_std','panic','path','pub','return',
    'saturating_add','saturating_mul','self','std','str','struct','test','to_le_bytes',
    'trait','true','type','u16','u32','u64','u8','unsafe_code','use','usize','while',
    'wrapping_add','wrapping_mul','b','v26','rs','_',
}


def 失敗(文: str) -> None:
    raise SystemExit(文)


def 区切り検査(道: Path, 文: str) -> None:
    対 = {')': '(', ']': '[', '}': '{'}
    山: list[str] = []
    文字列 = False
    文字 = False
    逃げ = False
    行注釈 = False
    塊注釈 = 0
    位置 = 0
    while 位置 < len(文):
        字 = 文[位置]
        次 = 文[位置 + 1] if 位置 + 1 < len(文) else ''
        if 行注釈:
            if 字 == '\n':
                行注釈 = False
            位置 += 1
            continue
        if 塊注釈:
            if 字 == '/' and 次 == '*':
                塊注釈 += 1
                位置 += 2
                continue
            if 字 == '*' and 次 == '/':
                塊注釈 -= 1
                位置 += 2
                continue
            位置 += 1
            continue
        if 文字列:
            if 逃げ:
                逃げ = False
            elif 字 == '\\':
                逃げ = True
            elif 字 == '"':
                文字列 = False
            位置 += 1
            continue
        if 文字:
            if 逃げ:
                逃げ = False
            elif 字 == '\\':
                逃げ = True
            elif 字 == "'":
                文字 = False
            位置 += 1
            continue
        if 字 == '/' and 次 == '/':
            行注釈 = True
            位置 += 2
            continue
        if 字 == '/' and 次 == '*':
            塊注釈 = 1
            位置 += 2
            continue
        if 字 == '"':
            文字列 = True
            位置 += 1
            continue
        if 字 == "'" and 位置 + 2 < len(文) and 文[位置 + 2] == "'":
            文字 = True
            位置 += 1
            continue
        if 字 in '([{':
            山.append(字)
        elif 字 in ')]}':
            if not 山 or 山[-1] != 対[字]:
                失敗(f'区切り不一致: {道}:{位置}')
            山.pop()
        位置 += 1
    if 山 or 文字列 or 塊注釈:
        失敗(f'未閉鎖区切り: {道}')


def 主() -> int:
    貨物 = (根 / 'Cargo.toml').read_text(encoding='utf-8')
    if 'version = "26.7.19"' not in 貨物:
        失敗('版が26.7.19ではない')
    if '[dependencies]\n' not in 貨物:
        失敗('依存節がない')
    依存部 = 貨物.split('[dependencies]\n', 1)[1].strip()
    if 依存部:
        失敗('外部依存が存在する')

    根源 = (根 / 'src/lib.rs').read_text(encoding='utf-8')
    if '#![no_std]' not in 根源:
        失敗('no_stdがない')
    if '#![forbid(unsafe_code)]' not in 根源:
        失敗('unsafe禁止がない')

    経路 = re.findall(r'#\[path = "([^"]+)"\]', 根源)
    for 相対 in 経路:
        if not (根 / 'src' / 相対).exists():
            失敗(f'モジュール欠落: {相対}')

    錆道 = sorted(根.rglob('*.rs'))
    if len(錆道) < 20:
        失敗('Rustファイルが少なすぎる')

    禁止 = ('todo!', 'unimplemented!', 'TODO', 'FIXME', 'unsafe {', 'unsafe fn')
    未許可英語: list[str] = []
    for 道 in 錆道:
        文 = 道.read_text(encoding='utf-8')
        if not any(ord(字) > 127 for 字 in 文):
            失敗(f'日本語文字がない: {道}')
        区切り検査(道, 文)
        for 語 in 禁止:
            if 語 in 文:
                失敗(f'禁止語 {語}: {道}')
        for 英語 in re.findall(r'[A-Za-z_][A-Za-z0-9_]*', 文):
            if 英語.startswith(('b0', 'b1', 'x', 'xcbf')) or 英語 in 許可語:
                continue
            未許可英語.append(f'{道.relative_to(根)}:{英語}')

    if 未許可英語:
        失敗('未許可英語: ' + ', '.join(sorted(set(未許可英語))))

    必須 = ('異常なら止まる', '後工程の引取りなしに補充しない', '選択と許可と実行を分ける')
    系譜 = (根 / 'src/系譜.rs').read_text(encoding='utf-8')
    for 語 in 必須:
        if 語 not in 系譜:
            失敗(f'不変条件欠落: {語}')

    print(f'構造検査合格: Rustファイル={len(錆道)} モジュール={len(経路)}')
    return 0


if __name__ == '__main__':
    sys.exit(主())
