use crate::語彙::{成否, 理由番号};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 通過;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 防止理由 {
    pub 理由番号: 理由番号,
}

pub trait 防止条件<入力> {
    fn 検査する(&self, 入力: &入力) -> 成否<通過, 防止理由>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 範囲防止 {
    pub 最小: u32,
    pub 最大: u32,
    pub 理由番号: 理由番号,
}

impl 防止条件<u32> for 範囲防止 {
    fn 検査する(&self, 入力: &u32) -> 成否<通過, 防止理由> {
        if *入力 < self.最小 || *入力 > self.最大 {
            成否::失敗(防止理由 { 理由番号: self.理由番号 })
        } else {
            成否::成功(通過)
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 一致防止 {
    pub 必要印: u64,
    pub 理由番号: 理由番号,
}

impl 防止条件<u64> for 一致防止 {
    fn 検査する(&self, 入力: &u64) -> 成否<通過, 防止理由> {
        if (*入力 & self.必要印) == self.必要印 {
            成否::成功(通過)
        } else {
            成否::失敗(防止理由 { 理由番号: self.理由番号 })
        }
    }
}
