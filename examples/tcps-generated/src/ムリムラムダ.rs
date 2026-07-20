use crate::語彙::{工程番号, 数量, 時刻, 有無};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 生産損失 {
    ムリ,
    ムラ,
    ムダ,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ムダ種類 {
    作り過ぎ,
    手待ち,
    運搬,
    加工そのもの,
    在庫,
    動作,
    不良,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 損失記録 {
    pub 工程: 工程番号,
    pub 生産損失: 生産損失,
    pub ムダ種類: 有無<ムダ種類>,
    pub 数量: 数量,
    pub 時刻: 時刻,
}

pub const fn 作り過ぎ記録(
    工程: 工程番号,
    生産数量: 数量,
    引取数量: 数量,
    時刻: 時刻,
) -> 有無<損失記録> {
    if 生産数量 > 引取数量 {
        有無::有る(損失記録 {
            工程,
            生産損失: 生産損失::ムダ,
            ムダ種類: 有無::有る(ムダ種類::作り過ぎ),
            数量: 生産数量 - 引取数量,
            時刻,
        })
    } else {
        有無::無い
    }
}
