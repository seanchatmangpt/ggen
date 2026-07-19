use crate::品質::異常;
use crate::語彙::{工程番号, 時刻, 有無};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 観察種別 {
    現物確認,
    作業確認,
    流れ確認,
    標準確認,
    異常確認,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 現地観察 {
    pub 工程: 工程番号,
    pub 種別: 観察種別,
    pub 時刻: 時刻,
    pub 異常: 有無<異常>,
    pub 観察者番号: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 観察立脚点 {
    未確認,
    現地確認済み(現地観察),
}

impl 観察立脚点 {
    pub const fn 現地確認済みか(&self) -> bool {
        matches!(self, Self::現地確認済み(_))
    }
}

