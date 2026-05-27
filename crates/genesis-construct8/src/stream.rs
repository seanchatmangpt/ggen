use crate::models::Construct8Packet;

/// A stream chunker that takes an iterator of triples and yields `Construct8Packet` chunks.
pub struct Construct8Chunker<I> {
    iter: I,
}

impl<I> Construct8Chunker<I> {
    pub fn new(iter: I) -> Self {
        Self { iter }
    }
}

impl<I> Iterator for Construct8Chunker<I>
where
    I: Iterator<Item = (u32, u32, u32)>,
{
    type Item = Construct8Packet;

    fn next(&mut self) -> Option<Self::Item> {
        let mut packet = Construct8Packet::new();
        let mut empty = true;

        while packet.len() < 8 {
            if let Some((s, p, o)) = self.iter.next() {
                let _ = packet.push(s, p, o);
                empty = false;
            } else {
                break;
            }
        }

        if empty {
            None
        } else {
            Some(packet)
        }
    }
}

/// Extension trait for iterators of triples to easily chunk them into `Construct8Packet`s.
pub trait Construct8StreamExt: Sized {
    fn into_construct8_chunks(self) -> Construct8Chunker<Self>;
}

impl<I> Construct8StreamExt for I
where
    I: Iterator<Item = (u32, u32, u32)>,
{
    fn into_construct8_chunks(self) -> Construct8Chunker<Self> {
        Construct8Chunker::new(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_construct8_streaming_chunks() {
        let triples = vec![
            (1, 10, 100),
            (2, 20, 200),
            (3, 30, 300),
            (4, 40, 400),
            (5, 50, 500),
            (6, 60, 600),
            (7, 70, 700),
            (8, 80, 800),
            (9, 90, 900),
            (10, 100, 1000),
        ];

        let chunks: Vec<Construct8Packet> = triples.into_iter().into_construct8_chunks().collect();

        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].len(), 8);
        assert_eq!(chunks[1].len(), 2);

        assert_eq!(chunks[0].subjects[0], 1);
        assert_eq!(chunks[0].subjects[7], 8);
        assert_eq!(chunks[1].subjects[0], 9);
        assert_eq!(chunks[1].subjects[1], 10);
    }
}
