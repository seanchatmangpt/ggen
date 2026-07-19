use crate::語彙::{周期, 工程番号, 数量, 成否, 有無, 標準番号};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 作業種別 {
    取る,
    置く,
    加工する,
    検査する,
    運ぶ,
    待つ,
    停止する,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 作業手順 {
    pub 順番: u8,
    pub 作業: 作業種別,
    pub 工程: 工程番号,
    pub 基準時間: 周期,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 標準手持ち {
    pub 数量: 数量,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 標準状態 {
    現行,
    廃止,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 標準満杯;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 標準作業<const 上限: usize> {
    pub 標準番号: 標準番号,
    pub 親標準: 有無<標準番号>,
    pub 工程: 工程番号,
    pub 周期: 周期,
    pub 標準手持ち: 標準手持ち,
    pub 状態: 標準状態,
    手順: [有無<作業手順>; 上限],
    手順数: usize,
}

impl<const 上限: usize> 標準作業<上限> {
    pub const fn 新規(
        標準番号: 標準番号,
        親標準: 有無<標準番号>,
        工程: 工程番号,
        周期: 周期,
        標準手持ち: 数量,
    ) -> Self {
        Self {
            標準番号,
            親標準,
            工程,
            周期,
            標準手持ち: 標準手持ち { 数量: 標準手持ち },
            状態: 標準状態::現行,
            手順: [有無::無い; 上限],
            手順数: 0,
        }
    }

    pub fn 手順を追加する(&mut self, 作業: 作業種別, 基準時間: 周期) -> 成否<usize, 標準満杯> {
        if self.手順数 >= 上限 {
            return 成否::失敗(標準満杯);
        }

        let 位置 = self.手順数;
        self.手順[位置] = 有無::有る(作業手順 {
            順番: 位置 as u8,
            作業,
            工程: self.工程,
            基準時間,
        });
        self.手順数 += 1;
        成否::成功(位置)
    }

    pub const fn 手順数(&self) -> usize {
        self.手順数
    }

    pub const fn 手順(&self, 位置: usize) -> 有無<作業手順> {
        if 位置 < self.手順数 {
            self.手順[位置]
        } else {
            有無::無い
        }
    }

    pub fn 廃止する(&mut self) {
        self.状態 = 標準状態::廃止;
    }

    pub const fn 現行か(&self) -> bool {
        matches!(self.状態, 標準状態::現行)
    }
}

