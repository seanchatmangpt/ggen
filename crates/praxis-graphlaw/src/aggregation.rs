use crate::encoding::Encoder;
use crate::utils::Utils;

fn clean_numeric_str(s: &str) -> String {
    let mut s = s.trim().to_string();
    if s.starts_with('<') && s.ends_with('>') {
        s = s[1..s.len() - 1].to_string();
    }
    if s.starts_with('"') && s.ends_with('"') {
        s = s[1..s.len() - 1].to_string();
    }
    s
}

pub trait Accumulator {
    fn add(&mut self, encoded_item: usize);
    fn get(&self) -> usize;
}

#[derive(Debug, Clone, Default)]
pub struct CountAccumulator {
    count: usize,
}

impl Accumulator for CountAccumulator {
    fn add(&mut self, _item: usize) {
        self.count += 1;
    }
    fn get(&self) -> usize {
        Encoder::add(self.count.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct SumAccumulator {
    sum: f64,
}

impl Accumulator for SumAccumulator {
    fn add(&mut self, item: usize) {
        if let Some(val) = Encoder::decode(&item) {
            let val = Utils::remove_literal_tags(&val);
            let val = clean_numeric_str(&val);
            self.sum += val.parse::<f64>().unwrap_or(0.0);
        }
    }
    fn get(&self) -> usize {
        Encoder::add(self.sum.to_string())
    }
}

impl Default for SumAccumulator {
    fn default() -> Self {
        SumAccumulator { sum: 0.0 }
    }
}

#[derive(Debug, Clone, Default)]
pub struct MinAccumulator {
    min: Option<f64>,
}

impl Accumulator for MinAccumulator {
    fn add(&mut self, item: usize) {
        if let Some(val) = Encoder::decode(&item) {
            let val = Utils::remove_literal_tags(&val);
            let val = clean_numeric_str(&val);
            if let Ok(num) = val.parse::<f64>() {
                self.min = Some(self.min.map_or(num, |m| m.min(num)));
            }
        }
    }
    fn get(&self) -> usize {
        Encoder::add(self.min.map_or("0".to_string(), |m| m.to_string()))
    }
}

#[derive(Debug, Clone, Default)]
pub struct MaxAccumulator {
    max: Option<f64>,
}

impl Accumulator for MaxAccumulator {
    fn add(&mut self, item: usize) {
        if let Some(val) = Encoder::decode(&item) {
            let val = Utils::remove_literal_tags(&val);
            let val = clean_numeric_str(&val);
            if let Ok(num) = val.parse::<f64>() {
                self.max = Some(self.max.map_or(num, |m| m.max(num)));
            }
        }
    }
    fn get(&self) -> usize {
        Encoder::add(self.max.map_or("0".to_string(), |m| m.to_string()))
    }
}

#[derive(Debug, Clone)]
pub struct AvgAccumulator {
    sum: f64,
    count: usize,
}

impl Accumulator for AvgAccumulator {
    fn add(&mut self, item: usize) {
        if let Some(val) = Encoder::decode(&item) {
            let val = Utils::remove_literal_tags(&val);
            let val = clean_numeric_str(&val);
            if let Ok(num) = val.parse::<f64>() {
                self.sum += num;
                self.count += 1;
            }
        }
    }
    fn get(&self) -> usize {
        let avg = if self.count > 0 {
            self.sum / self.count as f64
        } else {
            0.0
        };
        Encoder::add(avg.to_string())
    }
}

impl Default for AvgAccumulator {
    fn default() -> Self {
        AvgAccumulator { sum: 0.0, count: 0 }
    }
}

#[derive(Debug, Clone)]
pub enum AccumulatorImpl {
    Count(CountAccumulator),
    Sum(SumAccumulator),
    Min(MinAccumulator),
    Max(MaxAccumulator),
    Avg(AvgAccumulator),
}

impl Accumulator for AccumulatorImpl {
    fn add(&mut self, encoded_item: usize) {
        match self {
            Self::Count(acc) => acc.add(encoded_item),
            Self::Sum(acc) => acc.add(encoded_item),
            Self::Min(acc) => acc.add(encoded_item),
            Self::Max(acc) => acc.add(encoded_item),
            Self::Avg(acc) => acc.add(encoded_item),
        }
    }
    fn get(&self) -> usize {
        match self {
            Self::Count(acc) => acc.get(),
            Self::Sum(acc) => acc.get(),
            Self::Min(acc) => acc.get(),
            Self::Max(acc) => acc.get(),
            Self::Avg(acc) => acc.get(),
        }
    }
}
