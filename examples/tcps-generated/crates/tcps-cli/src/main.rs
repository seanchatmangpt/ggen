#![forbid(unsafe_code)]

use std::process::ExitCode;

use tcps_core::全体::豊田生産方式;
use tcps_std::実行環境;

fn 使い方() {
    println!("豊田コード生産方式 {}", tcps_core::版);
    println!("使い方: tcps <検査|環境|版>");
}

fn main() -> ExitCode {
    let 命令 = std::env::args().nth(1).unwrap_or_else(|| "検査".to_owned());
    match 命令.as_str() {
        "検査" => {
            let 方式 = 豊田生産方式::原初形();
            if 方式.二本柱が閉じている() {
                println!("成立: 自働化と必要時生産の二本柱は閉じています");
                ExitCode::SUCCESS
            } else {
                eprintln!("拒否: 二本柱が閉じていません");
                ExitCode::FAILURE
            }
        }
        "環境" => {
            let 環境 = 実行環境::現在();
            println!("操作体系={} 機械種別={} 版={}", 環境.操作体系, 環境.機械種別, 環境.版);
            ExitCode::SUCCESS
        }
        "版" => {
            println!("{} {}", tcps_core::版, tcps_core::配布種別);
            ExitCode::SUCCESS
        }
        _ => {
            使い方();
            ExitCode::from(2)
        }
    }
}

