use core::marker::PhantomData as 状態印;

use crate::品質::{品質判定, 異常票, 良品};
use crate::語彙::{工程番号, 数量, 時刻};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 稼働中;
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 停止中;
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 対策中;
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 再開可能;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 生産線<状態> {
    工程: 工程番号,
    完成数量: 数量,
    標準番号: u32,
    状態: 状態印<状態>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 作業結果 {
    良品完成 {
        生産線: 生産線<稼働中>,
        良品: 良品,
    },
    異常停止 {
        生産線: 生産線<停止中>,
        異常票: 異常票,
    },
}

impl 生産線<稼働中> {
    pub const fn 新設(工程: 工程番号, 標準番号: u32) -> Self {
        Self {
            工程,
            完成数量: 0,
            標準番号,
            状態: 状態印,
        }
    }

    pub fn 作業する(self, 判定: 品質判定, 現在時刻: 時刻) -> 作業結果 {
        match 判定 {
            品質判定::良 => {
                let 次数量 = self.完成数量.saturating_add(1);
                作業結果::良品完成 {
                    生産線: Self {
                        工程: self.工程,
                        完成数量: 次数量,
                        標準番号: self.標準番号,
                        状態: 状態印,
                    },
                    良品: 良品 {
                        工程: self.工程,
                        数量: 1,
                        完成時刻: 現在時刻,
                    },
                }
            }
            品質判定::異常(異常) => 作業結果::異常停止 {
                生産線: 生産線 {
                    工程: self.工程,
                    完成数量: self.完成数量,
                    標準番号: self.標準番号,
                    状態: 状態印,
                },
                異常票: 異常票 {
                    工程: self.工程,
                    異常,
                    発生時刻: 現在時刻,
                    発生数量: self.完成数量,
                },
            },
        }
    }
}

impl<状態> 生産線<状態> {
    pub const fn 工程(&self) -> 工程番号 {
        self.工程
    }

    pub const fn 完成数量(&self) -> 数量 {
        self.完成数量
    }

    pub const fn 標準番号(&self) -> u32 {
        self.標準番号
    }

    pub(crate) const fn 状態を変える<次状態>(self, 標準番号: u32) -> 生産線<次状態> {
        生産線 {
            工程: self.工程,
            完成数量: self.完成数量,
            標準番号,
            状態: 状態印,
        }
    }
}
