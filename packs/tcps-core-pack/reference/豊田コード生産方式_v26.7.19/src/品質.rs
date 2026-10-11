use crate::語彙::{工程番号, 数量, 時刻};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 異常 {
    糸切れ,
    品質不良,
    設備異常,
    作業遅れ,
    部品不足,
    標準逸脱,
    権限不足,
    時間超過,
    形状不適合,
    方策矛盾,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 品質判定 {
    良,
    異常(異常),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 異常票 {
    pub 工程: 工程番号,
    pub 異常: 異常,
    pub 発生時刻: 時刻,
    pub 発生数量: 数量,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 良品 {
    pub 工程: 工程番号,
    pub 数量: 数量,
    pub 完成時刻: 時刻,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 不良品 {
    pub 工程: 工程番号,
    pub 数量: 数量,
    pub 異常: 異常,
}
