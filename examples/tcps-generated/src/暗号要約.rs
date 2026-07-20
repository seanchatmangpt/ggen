use crate::受領証_impl::要約値;

const 初期状態: [u32; 8] = [
    0x6a09e667,
    0xbb67ae85,
    0x3c6ef372,
    0xa54ff53a,
    0x510e527f,
    0x9b05688c,
    0x1f83d9ab,
    0x5be0cd19,
];

const 定数: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct シャ二五六 {
    状態: [u32; 8],
    緩衝: [u8; 64],
    緩衝長: usize,
    総バイト数: u64,
}

impl Default for シャ二五六 {
    fn default() -> Self {
        Self::新規()
    }
}

impl シャ二五六 {
    #[must_use]
    pub const fn 新規() -> Self {
        Self {
            状態: 初期状態,
            緩衝: [0; 64],
            緩衝長: 0,
            総バイト数: 0,
        }
    }

    pub fn 更新する(&mut self, mut 入力: &[u8]) {
        self.総バイト数 = self.総バイト数.wrapping_add(入力.len() as u64);
        if self.緩衝長 != 0 {
            let 取込量 = core::cmp::min(64 - self.緩衝長, 入力.len());
            self.緩衝[self.緩衝長..self.緩衝長 + 取込量].copy_from_slice(&入力[..取込量]);
            self.緩衝長 += 取込量;
            入力 = &入力[取込量..];
            if self.緩衝長 == 64 {
                let 塊 = self.緩衝;
                self.圧縮する(&塊);
                self.緩衝長 = 0;
            }
        }
        while 入力.len() >= 64 {
            let mut 塊 = [0u8; 64];
            塊.copy_from_slice(&入力[..64]);
            self.圧縮する(&塊);
            入力 = &入力[64..];
        }
        if !入力.is_empty() {
            self.緩衝[..入力.len()].copy_from_slice(入力);
            self.緩衝長 = 入力.len();
        }
    }

    #[must_use]
    pub fn 完了する(mut self) -> 要約値 {
        let ビット長 = self.総バイト数.wrapping_mul(8);
        self.緩衝[self.緩衝長] = 0x80;
        self.緩衝長 += 1;
        if self.緩衝長 > 56 {
            self.緩衝[self.緩衝長..].fill(0);
            let 塊 = self.緩衝;
            self.圧縮する(&塊);
            self.緩衝 = [0; 64];
            self.緩衝長 = 0;
        }
        self.緩衝[self.緩衝長..56].fill(0);
        self.緩衝[56..64].copy_from_slice(&ビット長.to_be_bytes());
        let 塊 = self.緩衝;
        self.圧縮する(&塊);

        let mut 出力 = [0u8; 32];
        for (位置, 値) in self.状態.iter().enumerate() {
            出力[位置 * 4..位置 * 4 + 4].copy_from_slice(&値.to_be_bytes());
        }
        要約値(出力)
    }

    #[must_use]
    pub fn 要約する(入力: &[u8]) -> 要約値 {
        let mut 計算 = Self::新規();
        計算.更新する(入力);
        計算.完了する()
    }

    fn 圧縮する(&mut self, 塊: &[u8; 64]) {
        let mut 語 = [0u32; 64];
        for 位置 in 0..16 {
            let 起点 = 位置 * 4;
            語[位置] = u32::from_be_bytes([塊[起点], 塊[起点 + 1], 塊[起点 + 2], 塊[起点 + 3]]);
        }
        for 位置 in 16..64 {
            let 小一 = 語[位置 - 15].rotate_right(7) ^ 語[位置 - 15].rotate_right(18) ^ (語[位置 - 15] >> 3);
            let 小二 = 語[位置 - 2].rotate_right(17) ^ 語[位置 - 2].rotate_right(19) ^ (語[位置 - 2] >> 10);
            語[位置] = 語[位置 - 16]
                .wrapping_add(小一)
                .wrapping_add(語[位置 - 7])
                .wrapping_add(小二);
        }

        let mut 甲 = self.状態[0];
        let mut 乙 = self.状態[1];
        let mut 丙 = self.状態[2];
        let mut 丁 = self.状態[3];
        let mut 戊 = self.状態[4];
        let mut 己 = self.状態[5];
        let mut 庚 = self.状態[6];
        let mut 辛 = self.状態[7];

        for 位置 in 0..64 {
            let 大一 = 戊.rotate_right(6) ^ 戊.rotate_right(11) ^ 戊.rotate_right(25);
            let 選択 = (戊 & 己) ^ ((!戊) & 庚);
            let 一時一 = 辛
                .wrapping_add(大一)
                .wrapping_add(選択)
                .wrapping_add(定数[位置])
                .wrapping_add(語[位置]);
            let 大零 = 甲.rotate_right(2) ^ 甲.rotate_right(13) ^ 甲.rotate_right(22);
            let 多数 = (甲 & 乙) ^ (甲 & 丙) ^ (乙 & 丙);
            let 一時二 = 大零.wrapping_add(多数);
            辛 = 庚;
            庚 = 己;
            己 = 戊;
            戊 = 丁.wrapping_add(一時一);
            丁 = 丙;
            丙 = 乙;
            乙 = 甲;
            甲 = 一時一.wrapping_add(一時二);
        }

        self.状態[0] = self.状態[0].wrapping_add(甲);
        self.状態[1] = self.状態[1].wrapping_add(乙);
        self.状態[2] = self.状態[2].wrapping_add(丙);
        self.状態[3] = self.状態[3].wrapping_add(丁);
        self.状態[4] = self.状態[4].wrapping_add(戊);
        self.状態[5] = self.状態[5].wrapping_add(己);
        self.状態[6] = self.状態[6].wrapping_add(庚);
        self.状態[7] = self.状態[7].wrapping_add(辛);
    }
}

#[cfg(test)]
mod 試験 {
    use super::*;

    #[test]
    fn 空列の既知要約と一致する() {
        assert_eq!(
            シャ二五六::要約する(b"").0,
            [
                0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14,
                0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f, 0xb9, 0x24,
                0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c,
                0xa4, 0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55,
            ]
        );
    }

    #[test]
    fn 三文字の既知要約と一致する() {
        assert_eq!(
            シャ二五六::要約する(b"abc").0,
            [
                0xba, 0x78, 0x16, 0xbf, 0x8f, 0x01, 0xcf, 0xea,
                0x41, 0x41, 0x40, 0xde, 0x5d, 0xae, 0x22, 0x23,
                0xb0, 0x03, 0x61, 0xa3, 0x96, 0x17, 0x7a, 0x9c,
                0xb4, 0x10, 0xff, 0x61, 0xf2, 0x00, 0x15, 0xad,
            ]
        );
    }
}

