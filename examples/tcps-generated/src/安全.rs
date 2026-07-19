use crate::語彙::{成否, 理由番号};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 安全状態 {
    安全,
    危険,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 安全停止 {
    pub 理由番号: 理由番号,
}

pub const fn 安全を確認する(
    状態: 安全状態,
    理由番号: 理由番号,
) -> 成否<(), 安全停止> {
    match 状態 {
        安全状態::安全 => 成否::成功(()),
        安全状態::危険 => 成否::失敗(安全停止 { 理由番号 }),
    }
}

