#![deny(unsafe_op_in_unsafe_fn)]
#![deny(missing_debug_implementations)]

use tcps_core::正準::正準方策;
use tcps_core::自動選択::{作業領域, 選択する, 選択結果, 選択要求};

#[unsafe(export_name = "tcps_wasm_version_v1")]
pub extern "C" fn 版番号() -> u32 {
    tcps_core::機械可読版
}

#[unsafe(export_name = "tcps_wasm_select_canonical_v1")]
pub extern "C" fn 正準選択(
    取得権限: u64,
    準備完了: u64,
    最大時間: u32,
    時刻: u64,
) -> u64 {
    let 方策 = 正準方策();
    let mut 作業領域 = 作業領域::<3>::新規();
    let 結果 = 選択する(&方策, 選択要求 {
        取得権限,
        準備完了,
        最大時間,
        決定性必須: true,
        受領証必須: true,
        時刻,
    }, &mut 作業領域);

    match 結果 {
        選択結果::選択(提案) => {
            (1u64 << 63)
                | (u64::from(提案.道具) << 48)
                | (u64::from(提案.経路) << 32)
                | u64::from(提案.質量)
        }
        選択結果::拒否 { 理由, .. } => u64::from(理由.番号()),
    }
}

