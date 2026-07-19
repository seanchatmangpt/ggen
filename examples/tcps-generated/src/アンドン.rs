use crate::品質::異常;
use crate::語彙::{工程番号, 時刻, 成否, 有無};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 灯色 {
    緑,
    黄,
    赤,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 呼出し {
    pub 工程: 工程番号,
    pub 灯色: 灯色,
    pub 異常: 有無<異常>,
    pub 時刻: 時刻,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 呼出し満杯;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct アンドン盤<const 上限: usize> {
    呼出し: [有無<呼出し>; 上限],
    件数: usize,
}

impl<const 上限: usize> アンドン盤<上限> {
    pub const fn 新規() -> Self {
        Self {
            呼出し: [有無::無い; 上限],
            件数: 0,
        }
    }

    pub fn 点灯する(&mut self, 呼出し: 呼出し) -> 成否<usize, 呼出し満杯> {
        if self.件数 >= 上限 {
            return 成否::失敗(呼出し満杯);
        }
        let 位置 = self.件数;
        self.呼出し[位置] = 有無::有る(呼出し);
        self.件数 += 1;
        成否::成功(位置)
    }

    pub const fn 件数(&self) -> usize {
        self.件数
    }
}

