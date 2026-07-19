use crate::品質::異常;
use crate::語彙::{受領番号, 工程番号, 時刻, 有無, 標準番号, 道具番号, 理由番号};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 受領種別 {
    生産,
    停止,
    改善,
    標準更新,
    引取り,
    補充,
    選択,
    許可,
    実行,
    拒否,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 要約値(pub [u8; 32]);

impl 要約値 {
    pub const fn 零() -> Self {
        Self([0; 32])
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 受領証 {
    pub 受領番号: 受領番号,
    pub 種別: 受領種別,
    pub 工程: 工程番号,
    pub 時刻: 時刻,
    pub 標準番号: 標準番号,
    pub 異常: 有無<異常>,
    pub 道具: 有無<道具番号>,
    pub 理由: 有無<理由番号>,
    pub 前要約: 要約値,
    pub 後要約: 要約値,
}

pub const fn 簡易要約(入力: &[u8]) -> 要約値 {
    let mut 出力 = [0u8; 32];
    let mut 位置 = 0usize;
    let mut 状態 = 0xcbf29ce484222325u64;

    while 位置 < 入力.len() {
        状態 ^= 入力[位置] as u64;
        状態 = 状態.wrapping_mul(0x100000001b3);
        let 箱 = 位置 & 31;
        出力[箱] = 出力[箱].wrapping_add((状態 >> ((箱 & 7) * 8)) as u8);
        位置 += 1;
    }

    要約値(出力)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 受領台帳<const 上限: usize> {
    記録: [crate::語彙::有無<受領証>; 上限],
    件数: usize,
}

impl<const 上限: usize> 受領台帳<上限> {
    pub const fn 空() -> Self {
        Self {
            記録: [crate::語彙::有無::無い; 上限],
            件数: 0,
        }
    }

    pub fn 追加する(&mut self, 受領証: 受領証) -> crate::語彙::成否<usize, ()> {
        if self.件数 >= 上限 {
            return crate::語彙::成否::失敗(());
        }
        let 位置 = self.件数;
        self.記録[位置] = crate::語彙::有無::有る(受領証);
        self.件数 += 1;
        crate::語彙::成否::成功(位置)
    }

    pub const fn 件数(&self) -> usize {
        self.件数
    }
}

