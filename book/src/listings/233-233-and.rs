#![forbid(unsafe_code)]

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 稼働中;
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 停止中;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 品質判定 { 正常, 異常 }

pub struct 生産線<状態> { 工程番号: u16, 状態: core::marker::PhantomData<状態> }

impl 生産線<稼働中> {
    pub const fn 新設(工程番号: u16) -> Self {
        Self { 工程番号, 状態: core::marker::PhantomData }
    }

    pub fn 作業する(self, 判定: 品質判定) -> Result<Self, 生産線<停止中>> {
        match 判定 {
            品質判定::正常 => Ok(self),
            品質判定::異常 => Err(生産線 {
                工程番号: self.工程番号,
                状態: core::marker::PhantomData,
            }),
        }
    }
}

// Chapter 233: 233. 人間中心 and ムリムラムダ
