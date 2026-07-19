use crate::かんばん::かんばん;
use crate::語彙::{工程番号, 品目番号, 数量, 成否, 時刻};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 後工程要求 {
    pub 後工程: 工程番号,
    pub 品目: 品目番号,
    pub 必要数量: 数量,
    pub 必要時刻: 時刻,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 手持ち {
    pub 品目: 品目番号,
    pub 数量: 数量,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 補充指示 {
    pub 工程: 工程番号,
    pub 品目: 品目番号,
    pub 補充数量: 数量,
    pub 必要時刻: 時刻,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 不足 {
    pub 必要数量: 数量,
    pub 現在数量: 数量,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 作り過ぎ {
    pub 生産数量: 数量,
    pub 引取数量: 数量,
    pub 超過数量: 数量,
}

pub const fn かんばんを発行する(
    前工程: 工程番号,
    要求: 後工程要求,
    連番: u64,
) -> かんばん {
    かんばん::引取り(
        前工程,
        要求.後工程,
        要求.品目,
        要求.必要数量,
        要求.必要時刻,
        連番,
    )
}

pub fn 引き取る(
    mut 手持ち: 手持ち,
    札: かんばん,
) -> 成否<(手持ち, 補充指示), 不足> {
    if 手持ち.数量 < 札.数量 {
        return 成否::失敗(不足 {
            必要数量: 札.数量,
            現在数量: 手持ち.数量,
        });
    }

    手持ち.数量 -= 札.数量;
    成否::成功((
        手持ち,
        補充指示 {
            工程: 札.前工程,
            品目: 札.品目,
            補充数量: 札.数量,
            必要時刻: 札.必要時刻,
        },
    ))
}

pub const fn 作り過ぎを判定する(
    生産数量: 数量,
    引取数量: 数量,
) -> 成否<(), 作り過ぎ> {
    if 生産数量 > 引取数量 {
        成否::失敗(作り過ぎ {
            生産数量,
            引取数量,
            超過数量: 生産数量 - 引取数量,
        })
    } else {
        成否::成功(())
    }
}
