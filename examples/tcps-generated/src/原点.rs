use crate::語彙::{工程番号, 数量, 時刻};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 原点 {
    誰かのために,
    人を楽にする,
    人を中心に置く,
    良い品を早く安く届ける,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 生産目的 {
    pub 原点: 原点,
    pub 後工程: 工程番号,
    pub 必要数量: 数量,
    pub 必要時刻: 時刻,
}

impl 生産目的 {
    pub const fn 新規(
        原点: 原点,
        後工程: 工程番号,
        必要数量: 数量,
        必要時刻: 時刻,
    ) -> Self {
        Self {
            原点,
            後工程,
            必要数量,
            必要時刻,
        }
    }
}

