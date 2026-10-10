#![deny(unsafe_op_in_unsafe_fn)]
#![deny(missing_debug_implementations)]

use tcps_core::正準::正準方策;
use tcps_core::自動選択::{作業領域, 拒否理由, 選択する, 選択結果, 選択要求};
use tcps_core::語彙::有無;

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 外部選択要求 {
    pub 取得権限: u64,
    pub 準備完了: u64,
    pub 最大時間: u32,
    pub 決定性必須: u8,
    pub 受領証必須: u8,
    pub 予約: [u8; 2],
    pub 時刻: u64,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 外部選択結果 {
    pub 種別: u16,
    pub 理由: u16,
    pub 道具: u16,
    pub 経路: u16,
    pub 質量: u16,
    pub 予約: u16,
    pub 適格候補: u64,
    pub 準備候補: u64,
    pub 方策番号: u32,
}

#[unsafe(export_name = "tcps_version_v1")]
pub extern "C" fn 版番号() -> u32 {
    tcps_core::機械可読版
}

#[unsafe(export_name = "tcps_select_v1")]
pub extern "C" fn 選択する外部関数(要求: 外部選択要求) -> 外部選択結果 {
    let 方策 = 正準方策();
    let mut 作業領域 = 作業領域::<3>::新規();
    let 要求 = 選択要求 {
        取得権限: 要求.取得権限,
        準備完了: 要求.準備完了,
        最大時間: 要求.最大時間,
        決定性必須: 要求.決定性必須 != 0,
        受領証必須: 要求.受領証必須 != 0,
        時刻: 要求.時刻,
    };

    match 選択する(&方策, 要求, &mut 作業領域) {
        選択結果::選択(提案) => 外部選択結果 {
            種別: 1,
            理由: 0,
            道具: 提案.道具,
            経路: 提案.経路,
            質量: 提案.質量,
            予約: 0,
            適格候補: 提案.適格候補,
            準備候補: 提案.準備候補,
            方策番号: 提案.方策番号,
        },
        選択結果::拒否 { 理由, 適格候補, 準備候補, .. } => 外部選択結果 {
            種別: 2,
            理由: 理由.番号(),
            道具: 0,
            経路: 0,
            質量: 0,
            予約: 0,
            適格候補,
            準備候補,
            方策番号: 方策.方策番号,
        },
    }
}

#[must_use]
pub const fn 拒否理由番号(理由: 拒否理由) -> u16 {
    理由.番号()
}

#[must_use]
pub const fn 道具がある(値: u16) -> 有無<u16> {
    if 値 == 0 { 有無::無い } else { 有無::有る(値) }
}
