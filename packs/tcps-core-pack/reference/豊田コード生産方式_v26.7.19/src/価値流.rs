use crate::語彙::{工程番号, 成否, 有無};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 工程関係 {
    pub 前工程: 工程番号,
    pub 後工程: 工程番号,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 価値流満杯;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 価値流<const 上限: usize> {
    関係: [有無<工程関係>; 上限],
    件数: usize,
}

impl<const 上限: usize> 価値流<上限> {
    pub const fn 空() -> Self {
        Self {
            関係: [有無::無い; 上限],
            件数: 0,
        }
    }

    pub fn 接続する(
        &mut self,
        前工程: 工程番号,
        後工程: 工程番号,
    ) -> 成否<usize, 価値流満杯> {
        if self.件数 >= 上限 {
            return 成否::失敗(価値流満杯);
        }
        let 位置 = self.件数;
        self.関係[位置] = 有無::有る(工程関係 { 前工程, 後工程 });
        self.件数 += 1;
        成否::成功(位置)
    }

    pub const fn 次工程(
        &self,
        前工程: 工程番号,
    ) -> 有無<工程番号> {
        let mut 位置 = 0usize;
        while 位置 < self.件数 {
            if let 有無::有る(関係) = self.関係[位置] {
                if 関係.前工程 == 前工程 {
                    return 有無::有る(関係.後工程);
                }
            }
            位置 += 1;
        }
        有無::無い
    }

    pub const fn 件数(&self) -> usize {
        self.件数
    }
}
