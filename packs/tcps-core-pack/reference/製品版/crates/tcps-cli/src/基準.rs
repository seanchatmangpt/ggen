#![forbid(unsafe_code)]

use std::hint::black_box;
use std::time::Instant;

use tcps_core::正準::{正準方策, 正準要求};
use tcps_core::自動選択::{作業領域, 選択する, 選択結果};

fn main() {
    let 回数 = std::env::args()
        .nth(1)
        .and_then(|値| 値.parse::<u64>().ok())
        .unwrap_or(1_000_000);
    let 方策 = 正準方策();
    let mut 作業領域 = 作業領域::<3>::新規();

    for 時刻 in 0..10_000 {
        black_box(選択する(&方策, 正準要求(時刻), &mut 作業領域));
    }

    let 開始 = Instant::now();
    let mut 選択数 = 0u64;
    for 時刻 in 0..回数 {
        let 結果 = black_box(選択する(&方策, 正準要求(時刻), &mut 作業領域));
        if matches!(結果, 選択結果::選択(_)) {
            選択数 += 1;
        }
    }
    let 経過 = 開始.elapsed();
    let 一回ナノ秒 = 経過.as_nanos() as f64 / 回数 as f64;
    println!(
        "{{\"版\":\"{}\",\"回数\":{},\"選択数\":{},\"総ナノ秒\":{},\"一回ナノ秒\":{:.3}}}",
        tcps_core::版,
        回数,
        選択数,
        経過.as_nanos(),
        一回ナノ秒,
    );
}
