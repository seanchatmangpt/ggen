use crate::語彙::{数量, 周期, 成否};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 稼働可能時間 {
    pub 時間: 周期,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 顧客必要数 {
    pub 数量: 数量,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 必要数なし;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct タクト時間 {
    pub 周期: 周期,
}

pub const fn タクトを計算する(
    稼働可能時間: 稼働可能時間,
    顧客必要数: 顧客必要数,
) -> 成否<タクト時間, 必要数なし> {
    if 顧客必要数.数量 == 0 {
        return 成否::失敗(必要数なし);
    }

    成否::成功(タクト時間 {
        周期: 稼働可能時間.時間 / 顧客必要数.数量,
    })
}

