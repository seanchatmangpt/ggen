#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 人の役割 {
    機械を見張る,
    標準を実行する,
    異常を判断する,
    真因を調べる,
    標準を改善する,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 人間中心判定 {
    pub 機械監視から解放: bool,
    pub 問題解決へ集中: bool,
    pub 安全確保: bool,
    pub 技能向上: bool,
}

impl 人間中心判定 {
    pub const fn 成立する(&self) -> bool {
        self.機械監視から解放
            && self.問題解決へ集中
            && self.安全確保
            && self.技能向上
    }
}
