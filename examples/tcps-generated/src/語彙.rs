#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 有無<値> {
    有る(値),
    無い,
}

impl<値> 有無<値> {
    pub const fn 有るか(&self) -> bool {
        matches!(self, Self::有る(_))
    }

    pub const fn 無いか(&self) -> bool {
        !self.有るか()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 成否<成功, 失敗> {
    成功(成功),
    失敗(失敗),
}

impl<成功, 失敗> 成否<成功, 失敗> {
    pub const fn 成功か(&self) -> bool {
        matches!(self, Self::成功(_))
    }

    pub const fn 失敗か(&self) -> bool {
        !self.成功か()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 真偽 {
    真,
    偽,
}

impl 真偽 {
    pub const fn 値(self) -> bool {
        matches!(self, Self::真)
    }
}

pub type 数量 = u32;
pub type 時刻 = u64;
pub type 工程番号 = u16;
pub type 品目番号 = u32;
pub type 標準番号 = u32;
pub type 改善番号 = u32;
pub type 受領番号 = u64;
pub type 方策番号 = u32;
pub type 道具番号 = u16;
pub type 経路番号 = u16;
pub type 理由番号 = u16;
pub type 周期 = u32;
pub type 質量 = u16;
pub type 権限印 = u64;
pub type 準備印 = u64;
pub type 適格印 = u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 範囲外 {
    pub 値: u64,
    pub 上限: u64,
}

pub const fn 小さい方(左: u32, 右: u32) -> u32 {
    if 左 < 右 { 左 } else { 右 }
}

pub const fn 大きい方(左: u32, 右: u32) -> u32 {
    if 左 > 右 { 左 } else { 右 }
}
