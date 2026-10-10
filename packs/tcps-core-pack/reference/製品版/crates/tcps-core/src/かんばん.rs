use crate::語彙::{工程番号, 品目番号, 数量, 時刻};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum かんばん種別 {
    引取り,
    生産指示,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct かんばん {
    pub 種別: かんばん種別,
    pub 前工程: 工程番号,
    pub 後工程: 工程番号,
    pub 品目: 品目番号,
    pub 数量: 数量,
    pub 必要時刻: 時刻,
    pub 連番: u64,
}

impl かんばん {
    pub const fn 引取り(
        前工程: 工程番号,
        後工程: 工程番号,
        品目: 品目番号,
        数量: 数量,
        必要時刻: 時刻,
        連番: u64,
    ) -> Self {
        Self {
            種別: かんばん種別::引取り,
            前工程,
            後工程,
            品目,
            数量,
            必要時刻,
            連番,
        }
    }

    pub const fn 生産指示へ変える(self) -> Self {
        Self {
            種別: かんばん種別::生産指示,
            ..self
        }
    }
}
