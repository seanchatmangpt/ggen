#![no_std]
#![forbid(unsafe_code)]
#![deny(missing_debug_implementations)]

#[cfg(test)]
extern crate std;

#[cfg(test)]
#[path = "試験.rs"]
mod 試験;

#[path = "語彙.rs"]
pub mod 語彙;
#[path = "原点.rs"]
pub mod 原点;
#[path = "系譜.rs"]
pub mod 系譜;
#[path = "品質.rs"]
pub mod 品質;
#[path = "標準作業.rs"]
pub mod 標準作業;
#[path = "自働化.rs"]
pub mod 自働化;
#[path = "かんばん.rs"]
pub mod かんばん;
#[path = "必要時生産.rs"]
pub mod 必要時生産;
#[path = "平準化.rs"]
pub mod 平準化;
#[path = "アンドン.rs"]
pub mod アンドン;
#[path = "改善.rs"]
pub mod 改善;
#[path = "受領証.rs"]
pub mod 受領証;
#[path = "暗号要約.rs"]
pub mod 暗号要約;
#[path = "自動選択.rs"]
pub mod 自動選択;
#[path = "青い川のダム.rs"]
pub mod 青い川のダム;
#[path = "全体.rs"]
pub mod 全体;
#[path = "正準.rs"]
pub mod 正準;
#[path = "現地現物.rs"]
pub mod 現地現物;
#[path = "ポカヨケ.rs"]
pub mod ポカヨケ;
#[path = "五回なぜ.rs"]
pub mod 五回なぜ;
#[path = "人間中心.rs"]
pub mod 人間中心;
#[path = "ムリムラムダ.rs"]
pub mod ムリムラムダ;
#[path = "安全.rs"]
pub mod 安全;
#[path = "タクト.rs"]
pub mod タクト;
#[path = "価値流.rs"]
pub mod 価値流;
#[path = "工程能力.rs"]
pub mod 工程能力;

pub use アンドン::*;
pub use かんばん::*;
pub use 原点::*;
pub use 受領証::*;
pub use 暗号要約::*;
pub use 品質::*;
pub use 平準化::*;
pub use 必要時生産::*;
pub use 改善::*;
pub use 標準作業::*;
pub use 系譜::*;
pub use 自働化::*;
pub use 自動選択::*;
pub use 語彙::*;
pub use 青い川のダム::*;
pub use 全体::*;
pub use 正準::*;
pub use 現地現物::*;
pub use ポカヨケ::*;
pub use 五回なぜ::*;
pub use 人間中心::*;
pub use ムリムラムダ::*;
pub use 安全::*;
pub use タクト::*;
pub use 価値流::*;
pub use 工程能力::*;

pub const 版: &str = "v26.7.19";
pub const 配布種別: &str = "製品版";
pub const 機械可読版: u32 = 0x1A0713;
