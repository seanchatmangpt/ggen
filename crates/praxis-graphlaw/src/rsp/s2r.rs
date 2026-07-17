// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::should_implement_trait, dead_code)]

use log::debug; // Use log crate when building application
use std::collections::hash_set::{IntoIter, Iter};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::{channel, Sender};
use std::sync::{Arc, Mutex};
use std::thread;
use std::{f64, mem};

#[derive(Default)]
pub enum ReportStrategy {
    NonEmptyContent,
    OnContentChange,
    #[default]
    OnWindowClose,
    Periodic(usize),
}
#[derive(Default)]
pub enum Tick {
    #[default]
    TimeDriven,
    TupleDriven,
    BatchDriven,
}

pub struct Report<I>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send,
{
    strategies: Vec<ReportStrategy>,
    last_change: ContentContainer<I>,
}

impl<I> Report<I>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send,
{
    pub fn new() -> Report<I> {
        Report {
            strategies: Vec::new(),
            last_change: ContentContainer::new(),
        }
    }
    pub fn add(&mut self, strategy: ReportStrategy) {
        self.strategies.push(strategy);
    }
    pub fn report(&mut self, window: &Window, content: &ContentContainer<I>, ts: usize) -> bool {
        self.strategies.iter().all(|strategy| match strategy {
            ReportStrategy::NonEmptyContent => content.len() > 0,
            ReportStrategy::OnContentChange => {
                let comp = content.eq(&self.last_change);
                self.last_change = content.clone();
                comp
            }
            ReportStrategy::OnWindowClose => window.close < ts,
            ReportStrategy::Periodic(period) => ts.is_multiple_of(*period),
        })
    }
}

impl<I> Default for Report<I>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send,
{
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Eq, Hash, PartialEq, Debug, Clone)]
pub struct Window {
    open: usize,
    close: usize,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ContentContainer<I>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send,
{
    elements: HashSet<I>,
    last_timestamp_changed: usize,
}

impl<I> ContentContainer<I>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send,
{
    // `pub(crate)`, not private: widened so `rsp_test.rs` can build a real `ContentContainer` to
    // call `RSPEngine::evaluate_r2r_and_call_r2s` directly in a poison-recovery regression test,
    // rather than a test that only re-implements the same lock pattern standalone without
    // exercising the actual fixed function. No behavior change; visibility only.
    pub(crate) fn new() -> ContentContainer<I> {
        ContentContainer {
            elements: HashSet::new(),
            last_timestamp_changed: 0,
        }
    }
    fn len(&self) -> usize {
        self.elements.len()
    }
    pub(crate) fn add(&mut self, triple: I, ts: usize) {
        self.elements.insert(triple);
        self.last_timestamp_changed = ts;
    }
    pub fn get_last_timestamp_changed(&self) -> usize {
        self.last_timestamp_changed
    }

    pub fn iter(&self) -> Iter<'_, I> {
        self.elements.iter()
    }
    pub fn into_iter(mut self) -> IntoIter<I> {
        let map = mem::take(&mut self.elements);
        map.into_iter()
    }
}

pub struct CSPARQLWindow<I>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send,
{
    width: usize,
    slide: usize,
    t_0: usize,
    active_windows: HashMap<Window, ContentContainer<I>>,
    report: Report<I>,
    tick: Tick,
    app_time: usize,
    consumer: Option<Sender<ContentContainer<I>>>,
    call_back: Option<Box<dyn FnMut(ContentContainer<I>)>>,
}

impl<I> CSPARQLWindow<I>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send,
{
    pub fn new(width: usize, slide: usize, report: Report<I>, tick: Tick) -> CSPARQLWindow<I> {
        CSPARQLWindow {
            slide,
            width,
            t_0: 0,
            app_time: 0,
            report,
            consumer: None,
            active_windows: HashMap::new(),
            tick,
            call_back: None,
        }
    }
    pub fn add_to_window(&mut self, event_item: I, ts: usize) {
        let event_time = ts;
        self.scope(&event_time);

        let test = self
            .active_windows
            .clone()
            .into_iter()
            .filter_map(|(window, mut content)| {
                debug!(
                    "Processing Window [{:?}, {:?}) for element ({:?},{:?})",
                    window.open, window.close, event_item, ts
                );
                if window.open <= event_time && event_time <= window.close {
                    debug!(
                        "Adding element [{:?}] to Window [{:?},{:?})",
                        event_item, window.open, window.close
                    );
                    content.add(event_item.clone(), ts);
                    Some((window, content))
                } else {
                    debug!(
                        "Scheduling for Eviction [{:?},{:?})",
                        window.open, window.close
                    );
                    None
                }
            })
            .collect::<HashMap<Window, ContentContainer<I>>>();

        let max = self
            .active_windows
            .iter()
            .filter(|(window, content)| self.report.report(window, content, ts))
            .max_by(|(w1, _c1), (w2, _c2)| w1.close.cmp(&w2.close));
        if let Some(max_window) = max {
            if let Tick::TimeDriven = self.tick {
                if ts > self.app_time {
                    self.app_time = ts;
                    // notify consumers
                    debug!("Window triggers! {:?}", max_window);
                    // multithreaded consumer using channel
                    if let Some(sender) = &self.consumer {
                        // Receiver may have been dropped; window notification is best-effort.
                        let _ = sender.send(max_window.1.clone());
                    }
                    // single threaded consumer using callback
                    if let Some(call_back) = &mut self.call_back {
                        (call_back)(max_window.1.clone());
                    }
                }
            };
        }

        self.active_windows = test;
    }
    fn scope(&mut self, event_time: &usize) {
        // long c_sup = (long) Math.ceil(((double) Math.abs(t_e - t0) / (double) slide)) * slide;
        let _temp = (*event_time as f64 - self.t_0 as f64).abs();
        let _temp = ((*event_time as f64 - self.t_0 as f64).abs() / (self.slide as f64)).ceil();
        let c_sup = ((*event_time as f64 - self.t_0 as f64).abs() / (self.slide as f64)).ceil()
            * self.slide as f64;
        // long o_i = c_sup - width;
        let mut o_i = c_sup - self.width as f64;
        debug!(
            "Calculating the Windows to Open. First one opens at [{:?}] and closes at [{:?}]",
            o_i, c_sup
        );
        // log.debug("Calculating the Windows to Open. First one opens at [" + o_i + "] and closes at [" + c_sup + "]");
        //
        loop {
            debug!(
                "Computing Window [{:?},{:?}) if absent",
                o_i,
                (o_i + self.width as f64)
            );
            let window = Window {
                open: o_i as usize,
                close: (o_i + self.width as f64) as usize,
            };
            self.active_windows
                .entry(window)
                .or_insert_with(ContentContainer::new);
            o_i += self.slide as f64;
            if o_i > *event_time as f64 {
                break;
            }
        }
    }
    pub fn register(&mut self) -> Receiver<ContentContainer<I>> {
        let (send, recv) = channel::<ContentContainer<I>>();
        self.consumer.replace(send);
        recv
    }
    pub fn register_callback(&mut self, function: Box<dyn FnMut(ContentContainer<I>)>) {
        self.call_back.replace(function);
    }
    pub fn stop(&mut self) {
        self.consumer.take();
    }
}
struct ConsumerInner<I>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send,
{
    data: Mutex<Vec<ContentContainer<I>>>,
}
struct Consumer<I>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send,
{
    inner: Arc<ConsumerInner<I>>,
}
impl<I> Consumer<I>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send + 'static,
{
    fn new() -> Consumer<I> {
        Consumer {
            inner: Arc::new(ConsumerInner {
                data: Mutex::new(Vec::new()),
            }),
        }
    }
    fn start(&self, receiver: Receiver<ContentContainer<I>>) {
        let consumer_temp = self.inner.clone();
        thread::spawn(move || loop {
            match receiver.recv() {
                Ok(content) => {
                    debug!("Found graph {:?}", content);
                    consumer_temp
                        .data
                        .lock()
                        .unwrap_or_else(|e| e.into_inner())
                        .push(content);
                }
                Err(_) => {
                    debug!("Shutting down!");
                    break;
                }
            }
        });
    }
    fn len(&self) -> usize {
        self.inner
            .data
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .len()
    }
}
#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct WindowTriple {
    pub s: String,
    pub p: String,
    pub o: String,
}
#[cfg(test)]
#[path = "s2r_test.rs"]
mod s2r_test;
