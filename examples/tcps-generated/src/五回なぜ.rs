use crate::語彙::{成否, 有無};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct なぜ {
    pub 段: u8,
    pub 原因番号: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct なぜ満杯;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 五回なぜ {
    記録: [有無<なぜ>; 5],
    件数: usize,
}

impl 五回なぜ {
    pub const fn 新規() -> Self {
        Self {
            記録: [有無::無い; 5],
            件数: 0,
        }
    }

    pub fn 追加する(&mut self, 原因番号: u32) -> 成否<usize, なぜ満杯> {
        if self.件数 >= 5 {
            return 成否::失敗(なぜ満杯);
        }
        let 位置 = self.件数;
        self.記録[位置] = 有無::有る(なぜ {
            段: (位置 + 1) as u8,
            原因番号,
        });
        self.件数 += 1;
        成否::成功(位置)
    }

    pub const fn 件数(&self) -> usize {
        self.件数
    }

    pub const fn 最後(&self) -> 有無<なぜ> {
        if self.件数 == 0 {
            有無::無い
        } else {
            self.記録[self.件数 - 1]
        }
    }
}

