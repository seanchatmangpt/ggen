use crate::原点::原点;
use crate::系譜::{原初系譜, 系譜台帳};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 自働化柱 {
    pub 異常検知: bool,
    pub 異常停止: bool,
    pub 不良流出防止: bool,
    pub 人を番人にしない: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 必要時生産柱 {
    pub 必要なものだけ: bool,
    pub 必要な時だけ: bool,
    pub 必要な量だけ: bool,
    pub 停滞させない: bool,
    pub 売れた速さで作る: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct 豊田生産方式 {
    pub 原点: 原点,
    pub 自働化: 自働化柱,
    pub 必要時生産: 必要時生産柱,
    pub 系譜: 系譜台帳<8>,
}

impl 豊田生産方式 {
    pub const fn 原初形() -> Self {
        Self {
            原点: 原点::誰かのために,
            自働化: 自働化柱 {
                異常検知: true,
                異常停止: true,
                不良流出防止: true,
                人を番人にしない: true,
            },
            必要時生産: 必要時生産柱 {
                必要なものだけ: true,
                必要な時だけ: true,
                必要な量だけ: true,
                停滞させない: true,
                売れた速さで作る: true,
            },
            系譜: 原初系譜(),
        }
    }

    pub const fn 二本柱が閉じている(&self) -> bool {
        self.自働化.異常検知
            && self.自働化.異常停止
            && self.自働化.不良流出防止
            && self.自働化.人を番人にしない
            && self.必要時生産.必要なものだけ
            && self.必要時生産.必要な時だけ
            && self.必要時生産.必要な量だけ
            && self.必要時生産.停滞させない
            && self.必要時生産.売れた速さで作る
    }
}

