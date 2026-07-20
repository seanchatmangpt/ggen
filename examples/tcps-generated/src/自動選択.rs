use crate::受領証_impl::{受領種別, 受領証, 要約値};
use crate::暗号要約::シャ二五六;
use crate::語彙::{方策番号, 時刻, 有無, 権限印, 準備印, 理由番号, 質量, 道具番号, 適格印, 経路番号};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 認知品種 {
    正確なグラフ検索,
    決定的規則推論,
    近似意味検索,
    計画器,
    検証器,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 測度 {
    pub 意味適合: u8,
    pub 証拠適合: u8,
    pub 権限適合: u8,
    pub 時間適合: u8,
    pub 後工程適合: u8,
    pub 信頼度: u8,
    pub 費用適合: u8,
}

impl 測度 {
    pub const fn 最小値(self) -> u8 {
        let mut 値 = self.意味適合;
        if self.証拠適合 < 値 { 値 = self.証拠適合; }
        if self.権限適合 < 値 { 値 = self.権限適合; }
        if self.時間適合 < 値 { 値 = self.時間適合; }
        if self.後工程適合 < 値 { 値 = self.後工程適合; }
        if self.信頼度 < 値 { 値 = self.信頼度; }
        if self.費用適合 < 値 { 値 = self.費用適合; }
        値
    }

    pub const fn 乗法質量(self) -> 質量 {
        let 値 = self.最小値() as u16;
        値.saturating_mul(値)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 候補 {
    pub 道具: 道具番号,
    pub 経路: 経路番号,
    pub 品種: 認知品種,
    pub 必要権限: 権限印,
    pub 必要準備: 準備印,
    pub 決定的: bool,
    pub 受領証対応: bool,
    pub 予測時間: u32,
    pub 測度: 測度,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 方策<const 上限: usize> {
    pub 方策番号: 方策番号,
    pub 候補: [有無<候補>; 上限],
    pub 件数: usize,
    pub 方策要約: 要約値,
}

impl<const 上限: usize> 方策<上限> {
    pub const fn 空(方策番号: 方策番号, 方策要約: 要約値) -> Self {
        Self {
            方策番号,
            候補: [有無::無い; 上限],
            件数: 0,
            方策要約,
        }
    }

    pub fn 候補を追加する(&mut self, 候補: 候補) -> crate::語彙::成否<usize, ()> {
        if self.件数 >= 上限 {
            return crate::語彙::成否::失敗(());
        }
        let 位置 = self.件数;
        self.候補[位置] = 有無::有る(候補);
        self.件数 += 1;
        crate::語彙::成否::成功(位置)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 選択要求 {
    pub 取得権限: 権限印,
    pub 準備完了: 準備印,
    pub 最大時間: u32,
    pub 決定性必須: bool,
    pub 受領証必須: bool,
    pub 時刻: 時刻,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 拒否理由 {
    適格候補なし,
    準備候補なし,
    権限不足,
    決定性不足,
    受領証不足,
    時間超過,
    方策矛盾,
}

impl 拒否理由 {
    pub const fn 番号(self) -> 理由番号 {
        match self {
            Self::適格候補なし => 1,
            Self::準備候補なし => 2,
            Self::権限不足 => 3,
            Self::決定性不足 => 4,
            Self::受領証不足 => 5,
            Self::時間超過 => 6,
            Self::方策矛盾 => 7,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 選択提案 {
    pub 道具: 道具番号,
    pub 経路: 経路番号,
    pub 質量: 質量,
    pub 適格候補: 適格印,
    pub 準備候補: 準備印,
    pub 方策番号: 方策番号,
    pub 方策要約: 要約値,
    pub 受領証: 受領証,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum 選択結果 {
    選択(選択提案),
    拒否 {
        理由: 拒否理由,
        適格候補: 適格印,
        準備候補: 準備印,
        受領証: 受領証,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 作業領域<const 上限: usize> {
    pub 質量: [質量; 上限],
}

impl<const 上限: usize> 作業領域<上限> {
    pub const fn 新規() -> Self {
        Self { 質量: [0; 上限] }
    }
}

pub fn 選択する<const 上限: usize>(
    方策: &方策<上限>,
    要求: 選択要求,
    作業領域: &mut 作業領域<上限>,
) -> 選択結果 {
    let mut 適格候補 = 0u64;
    let mut 準備候補 = 0u64;
    let mut 最良位置 = 有無::無い;
    let mut 最良質量 = 0u16;
    let mut 位置 = 0usize;

    while 位置 < 方策.件数 {
        if let 有無::有る(候補) = 方策.候補[位置] {
            let 権限適合 = (要求.取得権限 & 候補.必要権限) == 候補.必要権限;
            let 決定性適合 = !要求.決定性必須 || 候補.決定的;
            let 受領証適合 = !要求.受領証必須 || 候補.受領証対応;
            let 時間適合 = 候補.予測時間 <= 要求.最大時間;
            let 適格 = 権限適合 && 決定性適合 && 受領証適合 && 時間適合;
            let 準備 = 適格 && (要求.準備完了 & 候補.必要準備) == 候補.必要準備;
            let 印 = 1u64 << 位置;

            if 適格 {
                適格候補 |= 印;
            }
            if 準備 {
                準備候補 |= 印;
                let 質量 = 候補.測度.乗法質量();
                作業領域.質量[位置] = 質量;
                if 質量 > 最良質量 || (質量 == 最良質量 && 最良位置.無いか()) {
                    最良質量 = 質量;
                    最良位置 = 有無::有る(位置);
                }
            }
        }
        位置 += 1;
    }

    let 入力要約 = 要求要約(要求);

    match 最良位置 {
        有無::有る(最良位置) => {
            let 候補 = match 方策.候補[最良位置] {
                有無::有る(候補) => 候補,
                有無::無い => {
                    return 拒否を作る(
                        方策,
                        要求,
                        拒否理由::方策矛盾,
                        適格候補,
                        準備候補,
                        入力要約,
                    );
                }
            };
            let 受領証 = 受領証 {
                受領番号: 要求.時刻,
                種別: 受領種別::選択,
                工程: 0,
                時刻: 要求.時刻,
                標準番号: 方策.方策番号,
                異常: 有無::無い,
                道具: 有無::有る(候補.道具),
                理由: 有無::無い,
                前要約: 入力要約,
                後要約: 方策.方策要約,
            };
            選択結果::選択(選択提案 {
                道具: 候補.道具,
                経路: 候補.経路,
                質量: 最良質量,
                適格候補,
                準備候補,
                方策番号: 方策.方策番号,
                方策要約: 方策.方策要約,
                受領証,
            })
        }
        有無::無い => {
            let 理由 = if 適格候補 == 0 {
                拒否理由::適格候補なし
            } else {
                拒否理由::準備候補なし
            };
            拒否を作る(
                方策,
                要求,
                理由,
                適格候補,
                準備候補,
                入力要約,
            )
        }
    }
}

fn 要求要約(要求: 選択要求) -> 要約値 {
    let mut 要約 = シャ二五六::新規();
    要約.更新する(&要求.取得権限.to_le_bytes());
    要約.更新する(&要求.準備完了.to_le_bytes());
    要約.更新する(&要求.最大時間.to_le_bytes());
    要約.更新する(&[要求.決定性必須 as u8]);
    要約.更新する(&[要求.受領証必須 as u8]);
    要約.更新する(&要求.時刻.to_le_bytes());
    要約.完了する()
}

fn 拒否を作る<const 上限: usize>(
    方策: &方策<上限>,
    要求: 選択要求,
    理由: 拒否理由,
    適格候補: 適格印,
    準備候補: 準備印,
    入力要約: 要約値,
) -> 選択結果 {
    選択結果::拒否 {
        理由,
        適格候補,
        準備候補,
        受領証: 受領証 {
            受領番号: 要求.時刻,
            種別: 受領種別::拒否,
            工程: 0,
            時刻: 要求.時刻,
            標準番号: 方策.方策番号,
            異常: 有無::無い,
            道具: 有無::無い,
            理由: 有無::有る(理由.番号()),
            前要約: 入力要約,
            後要約: 方策.方策要約,
        },
    }
}
