use crate::受領証_impl::{受領種別, 受領証};
use crate::自動選択::選択提案;
use crate::語彙::{成否, 時刻, 有無, 権限印, 道具番号};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 能力札 {
    pub 許可道具: 道具番号,
    pub 権限: 権限印,
    pub 有効期限: 時刻,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 許可済み選択 {
    提案: 選択提案,
    能力札: 能力札,
    許可受領証: 受領証,
}

impl 許可済み選択 {
    pub const fn 道具(&self) -> 道具番号 {
        self.提案.道具
    }

    pub const fn 提案(&self) -> 選択提案 {
        self.提案
    }

    pub const fn 許可受領証(&self) -> 受領証 {
        self.許可受領証
    }

    pub const fn 能力札(&self) -> 能力札 {
        self.能力札
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 許可拒否 {
    道具不一致,
    期限切れ,
    権限なし,
    方策不一致,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 仲介者 {
    pub 必要権限: 権限印,
    pub 方策要約: crate::受領証_impl::要約値,
}

impl 仲介者 {
    pub fn 許可する(
        &self,
        提案: 選択提案,
        能力札: 能力札,
        現在時刻: 時刻,
    ) -> 成否<許可済み選択, 許可拒否> {
        if 能力札.許可道具 != 提案.道具 {
            return 成否::失敗(許可拒否::道具不一致);
        }
        if 現在時刻 > 能力札.有効期限 {
            return 成否::失敗(許可拒否::期限切れ);
        }
        if (能力札.権限 & self.必要権限) != self.必要権限 {
            return 成否::失敗(許可拒否::権限なし);
        }
        if 提案.方策要約 != self.方策要約 {
            return 成否::失敗(許可拒否::方策不一致);
        }

        let 許可受領証 = 受領証 {
            受領番号: 現在時刻,
            種別: 受領種別::許可,
            工程: 0,
            時刻: 現在時刻,
            標準番号: 提案.方策番号,
            異常: 有無::無い,
            道具: 有無::有る(提案.道具),
            理由: 有無::無い,
            前要約: 提案.受領証.後要約,
            後要約: self.方策要約,
        };

        成否::成功(許可済み選択 {
            提案,
            能力札,
            許可受領証,
        })
    }
}

pub trait 実行器 {
    type 成果;
    type 失敗;

    fn 実行する(
        &mut self,
        許可: 許可済み選択,
        現在時刻: 時刻,
    ) -> 成否<(Self::成果, 受領証), Self::失敗>;
}

