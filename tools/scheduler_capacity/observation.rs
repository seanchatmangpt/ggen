#[derive(Clone,Debug)]
pub struct CycleObservation { pub cycle:u64, pub app:&'static str, pub operation:&'static str, pub outcome:&'static str }
pub fn monotonic(xs:&[CycleObservation])->bool {
 xs.windows(2).all(|w|w[0].cycle<w[1].cycle)
}
pub fn highest(xs:&[CycleObservation])->u64 {
 xs.iter().map(|x|x.cycle).max().unwrap_or(0)
}
pub fn count_app(xs:&[CycleObservation],app:&str)->usize {
 xs.iter().filter(|x|x.app==app).count()
}
