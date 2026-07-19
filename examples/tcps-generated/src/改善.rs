use crate::品質::異常票;
use crate::標準作業_impl::標準作業;
use crate::自働化::{停止中, 再開可能, 対策中, 生産線, 稼働中};
use crate::語彙::{改善番号, 成否, 標準番号};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 真因 {
    標準不足,
    設計不足,
    検知不足,
    教育不足,
    部品供給不足,
    工程能力不足,
    権限設計不足,
    方策設計不足,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 対策 {
    標準変更,
    検知追加,
    ポカヨケ追加,
    工程分割,
    能力増強,
    供給順序変更,
    権限境界追加,
    方策再コンパイル,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 改善票 {
    pub 改善番号: 改善番号,
    pub 元異常: 異常票,
    pub 真因: 真因,
    pub 対策: 対策,
    pub 旧標準: 標準番号,
    pub 新標準: 標準番号,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 標準不一致 {
    pub 生産線標準: 標準番号,
    pub 改善旧標準: 標準番号,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 対策工程 {
    pub 生産線: 生産線<対策中>,
    pub 改善票: 改善票,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 再開工程 {
    pub 生産線: 生産線<再開可能>,
    pub 改善票: 改善票,
}

impl 生産線<停止中> {
    pub fn 真因対策を登録する(
        self,
        改善票: 改善票,
    ) -> 成否<対策工程, 標準不一致> {
        if self.標準番号() != 改善票.旧標準 {
            return 成否::失敗(標準不一致 {
                生産線標準: self.標準番号(),
                改善旧標準: 改善票.旧標準,
            });
        }

        成否::成功(対策工程 {
            生産線: self.状態を変える(改善票.旧標準),
            改善票,
        })
    }
}

impl 対策工程 {
    pub const fn 対策を完了する(self) -> 再開工程 {
        再開工程 {
            生産線: self.生産線.状態を変える(self.改善票.新標準),
            改善票: self.改善票,
        }
    }
}

impl 再開工程 {
    pub fn 標準を更新して再開する<const 上限: usize>(
        self,
        旧標準: &mut 標準作業<上限>,
        新標準: &標準作業<上限>,
    ) -> 成否<生産線<稼働中>, 標準不一致> {
        if 旧標準.標準番号 != self.改善票.旧標準
            || 新標準.標準番号 != self.改善票.新標準
            || 新標準.親標準 != crate::語彙::有無::有る(旧標準.標準番号)
        {
            return 成否::失敗(標準不一致 {
                生産線標準: self.改善票.新標準,
                改善旧標準: 旧標準.標準番号,
            });
        }

        旧標準.廃止する();
        成否::成功(self.生産線.状態を変える(新標準.標準番号))
    }
}

