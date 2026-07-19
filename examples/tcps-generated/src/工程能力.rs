use crate::語彙::{数量, 周期, 成否};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 工程能力 {
    pub 最大数量: 数量,
    pub 最小周期: 周期,
    pub 最大候補数: u16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 能力要求 {
    pub 必要数量: 数量,
    pub 必要周期: 周期,
    pub 候補数: u16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 能力超過 {
    数量超過,
    周期不足,
    候補過多,
}

pub const fn 能力を判定する(
    能力: 工程能力,
    要求: 能力要求,
) -> 成否<(), 能力超過> {
    if 要求.必要数量 > 能力.最大数量 {
        return 成否::失敗(能力超過::数量超過);
    }
    if 要求.必要周期 < 能力.最小周期 {
        return 成否::失敗(能力超過::周期不足);
    }
    if 要求.候補数 > 能力.最大候補数 {
        return 成否::失敗(能力超過::候補過多);
    }
    成否::成功(())
}

