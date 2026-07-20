#![forbid(unsafe_code)]
#![deny(missing_debug_implementations)]

use std::fs::{self, File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use tcps_core::受領証_impl::{受領証, 受領種別, 要約値};
use tcps_core::語彙::{有無, 理由番号, 道具番号};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct 実行環境 {
    pub 操作体系: String,
    pub 機械種別: String,
    pub 処理系: String,
    pub 版: String,
}

impl 実行環境 {
    #[must_use]
    pub fn 現在() -> Self {
        Self {
            操作体系: std::env::consts::OS.to_owned(),
            機械種別: std::env::consts::ARCH.to_owned(),
            処理系: "rust".to_owned(),
            版: tcps_core::版.to_owned(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct 永続化結果 {
    pub 経路: PathBuf,
    pub バイト数: u64,
}

pub fn 現在時刻() -> io::Result<u64> {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|値| 値.as_secs())
        .map_err(io::Error::other)
}

#[must_use]
pub fn 十六進(要約: 要約値) -> String {
    const 表: &[u8; 16] = b"0123456789abcdef";
    let mut 出力 = String::with_capacity(64);
    for 値 in 要約.0 {
        出力.push(char::from(表[usize::from(値 >> 4)]));
        出力.push(char::from(表[usize::from(値 & 0x0f)]));
    }
    出力
}

fn 種別名(種別: 受領種別) -> &'static str {
    match 種別 {
        受領種別::生産 => "生産",
        受領種別::停止 => "停止",
        受領種別::改善 => "改善",
        受領種別::標準更新 => "標準更新",
        受領種別::引取り => "引取り",
        受領種別::補充 => "補充",
        受領種別::選択 => "選択",
        受領種別::許可 => "許可",
        受領種別::実行 => "実行",
        受領種別::拒否 => "拒否",
    }
}

fn 道具値(値: 有無<道具番号>) -> String {
    match 値 {
        有無::有る(番号) => 番号.to_string(),
        有無::無い => "null".to_owned(),
    }
}

fn 理由値(値: 有無<理由番号>) -> String {
    match 値 {
        有無::有る(番号) => 番号.to_string(),
        有無::無い => "null".to_owned(),
    }
}

#[must_use]
pub fn 受領証を文字列化する(受領証: &受領証) -> String {
    format!(
        concat!(
            "{{\n",
            "  \"受領番号\": {},\n",
            "  \"種別\": \"{}\",\n",
            "  \"工程\": {},\n",
            "  \"時刻\": {},\n",
            "  \"標準番号\": {},\n",
            "  \"道具\": {},\n",
            "  \"理由\": {},\n",
            "  \"前要約\": \"{}\",\n",
            "  \"後要約\": \"{}\"\n",
            "}}\n"
        ),
        受領証.受領番号,
        種別名(受領証.種別),
        受領証.工程,
        受領証.時刻,
        受領証.標準番号,
        道具値(受領証.道具),
        理由値(受領証.理由),
        十六進(受領証.前要約),
        十六進(受領証.後要約),
    )
}

pub fn 受領証を保存する(根: &Path, 受領証: &受領証) -> io::Result<永続化結果> {
    fs::create_dir_all(根)?;
    let 経路 = 根.join(format!("受領証-{}.json", 受領証.受領番号));
    let 一時 = 根.join(format!(".受領証-{}.tmp", 受領証.受領番号));
    let 内容 = 受領証を文字列化する(受領証);

    let mut ファイル = File::create(&一時)?;
    ファイル.write_all(内容.as_bytes())?;
    ファイル.sync_all()?;
    fs::rename(&一時, &経路)?;

    Ok(永続化結果 {
        経路,
        バイト数: 内容.len() as u64,
    })
}

#[cfg(test)]
mod 試験 {
    use super::*;
    use tcps_core::品質::異常;
    use tcps_core::語彙::有無;

    #[test]
    fn 受領証文字列は日本語項目を保つ() {
        let 受領証 = 受領証 {
            受領番号: 1,
            種別: 受領種別::停止,
            工程: 2,
            時刻: 3,
            標準番号: 4,
            異常: 有無::有る(異常::品質不良),
            道具: 有無::無い,
            理由: 有無::有る(7),
            前要約: 要約値::零(),
            後要約: 要約値::零(),
        };
        let 内容 = 受領証を文字列化する(&受領証);
        assert!(内容.contains("停止"));
        assert!(内容.contains("受領番号"));
    }
}

