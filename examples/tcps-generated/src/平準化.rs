use crate::語彙::{工程番号, 品目番号, 数量, 成否, 有無};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 生産枠 {
    pub 順番: u16,
    pub 工程: 工程番号,
    pub 品目: 品目番号,
    pub 数量: 数量,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 平準化箱満杯;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 平準化箱<const 上限: usize> {
    枠: [有無<生産枠>; 上限],
    件数: usize,
}

impl<const 上限: usize> 平準化箱<上限> {
    pub const fn 空() -> Self {
        Self {
            枠: [有無::無い; 上限],
            件数: 0,
        }
    }

    pub fn 追加する(
        &mut self,
        工程: 工程番号,
        品目: 品目番号,
        数量: 数量,
    ) -> 成否<usize, 平準化箱満杯> {
        if self.件数 >= 上限 {
            return 成否::失敗(平準化箱満杯);
        }

        let 位置 = self.件数;
        self.枠[位置] = 有無::有る(生産枠 {
            順番: 位置 as u16,
            工程,
            品目,
            数量,
        });
        self.件数 += 1;
        成否::成功(位置)
    }

    pub const fn 件数(&self) -> usize {
        self.件数
    }

    pub const fn 取得(&self, 位置: usize) -> 有無<生産枠> {
        if 位置 < self.件数 {
            self.枠[位置]
        } else {
            有無::無い
        }
    }
}

