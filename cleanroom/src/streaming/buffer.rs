//! Efficient buffering for streaming operations
//!
//! This module provides efficient buffering mechanisms for streaming operations,
//! including ring buffers, circular buffers, and memory-mapped buffers.

use crate::error::Result;
use std::sync::Arc;
use tokio::sync::Mutex;

/// Ring buffer for efficient circular buffering
pub struct RingBuffer {
    data: Vec<u8>,
    head: usize,
    tail: usize,
    size: usize,
    capacity: usize,
}

impl RingBuffer {
    /// Create a new ring buffer with specified capacity
    pub fn new(capacity: usize) -> Self {
        Self {
            data: vec![0; capacity],
            head: 0,
            tail: 0,
            size: 0,
            capacity,
        }
    }

    /// Create a ring buffer with default capacity
    pub fn default() -> Self {
        Self::new(8192)
    }

    /// Write data to the buffer
    pub fn write(&mut self, data: &[u8]) -> Result<usize> {
        let available = self.capacity - self.size;
        let to_write = std::cmp::min(data.len(), available);
        
        if to_write == 0 {
            return Ok(0);
        }
        
        // Write data starting from tail
        let mut written = 0;
        while written < to_write {
            let space_until_end = self.capacity - self.tail;
            let can_write = std::cmp::min(to_write - written, space_until_end);
            
            self.data[self.tail..self.tail + can_write].copy_from_slice(&data[written..written + can_write]);
            
            self.tail = (self.tail + can_write) % self.capacity;
            written += can_write;
        }
        
        self.size += written;
        Ok(written)
    }

    /// Read data from the buffer
    pub fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let to_read = std::cmp::min(buf.len(), self.size);
        
        if to_read == 0 {
            return Ok(0);
        }
        
        // Read data starting from head
        let mut read = 0;
        while read < to_read {
            let space_until_end = self.capacity - self.head;
            let can_read = std::cmp::min(to_read - read, space_until_end);
            
            buf[read..read + can_read].copy_from_slice(&self.data[self.head..self.head + can_read]);
            
            self.head = (self.head + can_read) % self.capacity;
            read += can_read;
        }
        
        self.size -= read;
        Ok(read)
    }

    /// Peek at data without consuming it
    pub fn peek(&self, buf: &mut [u8]) -> Result<usize> {
        let to_read = std::cmp::min(buf.len(), self.size);
        
        if to_read == 0 {
            return Ok(0);
        }
        
        // Peek data starting from head
        let mut read = 0;
        let mut head = self.head;
        while read < to_read {
            let space_until_end = self.capacity - head;
            let can_read = std::cmp::min(to_read - read, space_until_end);
            
            buf[read..read + can_read].copy_from_slice(&self.data[head..head + can_read]);
            
            head = (head + can_read) % self.capacity;
            read += can_read;
        }
        
        Ok(read)
    }

    /// Get the current size
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get the capacity
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Check if buffer is empty
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    /// Check if buffer is full
    pub fn is_full(&self) -> bool {
        self.size == self.capacity
    }

    /// Get available space
    pub fn available_space(&self) -> usize {
        self.capacity - self.size
    }

    /// Clear the buffer
    pub fn clear(&mut self) {
        self.head = 0;
        self.tail = 0;
        self.size = 0;
    }

    /// Get buffer utilization percentage
    pub fn utilization(&self) -> f64 {
        (self.size as f64 / self.capacity as f64) * 100.0
    }
}

impl Default for RingBuffer {
    fn default() -> Self {
        Self::default()
    }
}

/// Circular buffer with automatic growth
pub struct CircularBuffer {
    data: Vec<u8>,
    head: usize,
    tail: usize,
    size: usize,
    capacity: usize,
    growth_factor: f64,
}

impl CircularBuffer {
    /// Create a new circular buffer
    pub fn new(initial_capacity: usize) -> Self {
        Self {
            data: vec![0; initial_capacity],
            head: 0,
            tail: 0,
            size: 0,
            capacity: initial_capacity,
            growth_factor: 1.5,
        }
    }

    /// Create a circular buffer with default capacity
    pub fn default() -> Self {
        Self::new(8192)
    }

    /// Set growth factor for automatic resizing
    pub fn with_growth_factor(mut self, factor: f64) -> Self {
        self.growth_factor = factor;
        self
    }

    /// Write data to the buffer
    pub fn write(&mut self, data: &[u8]) -> Result<usize> {
        let needed_space = data.len();
        
        // Grow buffer if needed
        if self.size + needed_space > self.capacity {
            self.grow(needed_space)?;
        }
        
        // Write data starting from tail
        let mut written = 0;
        while written < needed_space {
            let space_until_end = self.capacity - self.tail;
            let can_write = std::cmp::min(needed_space - written, space_until_end);
            
            self.data[self.tail..self.tail + can_write].copy_from_slice(&data[written..written + can_write]);
            
            self.tail = (self.tail + can_write) % self.capacity;
            written += can_write;
        }
        
        self.size += written;
        Ok(written)
    }

    /// Read data from the buffer
    pub fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let to_read = std::cmp::min(buf.len(), self.size);
        
        if to_read == 0 {
            return Ok(0);
        }
        
        // Read data starting from head
        let mut read = 0;
        while read < to_read {
            let space_until_end = self.capacity - self.head;
            let can_read = std::cmp::min(to_read - read, space_until_end);
            
            buf[read..read + can_read].copy_from_slice(&self.data[self.head..self.head + can_read]);
            
            self.head = (self.head + can_read) % self.capacity;
            read += can_read;
        }
        
        self.size -= read;
        Ok(read)
    }

    /// Grow the buffer to accommodate more data
    fn grow(&mut self, needed_space: usize) -> Result<()> {
        let new_capacity = ((self.capacity as f64 * self.growth_factor) as usize)
            .max(self.capacity + needed_space);
        
        let mut new_data = vec![0; new_capacity];
        
        // Copy existing data to new buffer
        if self.size > 0 {
            if self.head < self.tail {
                // Data is contiguous
                new_data[..self.size].copy_from_slice(&self.data[self.head..self.tail]);
            } else {
                // Data wraps around
                let first_part = self.capacity - self.head;
                new_data[..first_part].copy_from_slice(&self.data[self.head..]);
                new_data[first_part..self.size].copy_from_slice(&self.data[..self.tail]);
            }
        }
        
        self.data = new_data;
        self.capacity = new_capacity;
        self.head = 0;
        self.tail = self.size;
        
        Ok(())
    }

    /// Get the current size
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get the capacity
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Check if buffer is empty
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    /// Get available space
    pub fn available_space(&self) -> usize {
        self.capacity - self.size
    }

    /// Clear the buffer
    pub fn clear(&mut self) {
        self.head = 0;
        self.tail = 0;
        self.size = 0;
    }

    /// Get buffer utilization percentage
    pub fn utilization(&self) -> f64 {
        (self.size as f64 / self.capacity as f64) * 100.0
    }
}

impl Default for CircularBuffer {
    fn default() -> Self {
        Self::default()
    }
}

/// Memory-mapped buffer for large data handling
pub struct MemoryMappedBuffer {
    data: Arc<Mutex<Vec<u8>>>,
    size: usize,
    capacity: usize,
}

impl MemoryMappedBuffer {
    /// Create a new memory-mapped buffer
    pub fn new(capacity: usize) -> Self {
        Self {
            data: Arc::new(Mutex::new(vec![0; capacity])),
            size: 0,
            capacity,
        }
    }

    /// Create a memory-mapped buffer with default capacity
    pub fn default() -> Self {
        Self::new(1024 * 1024) // 1MB default
    }

    /// Write data to the buffer
    pub async fn write(&mut self, data: &[u8]) -> Result<usize> {
        let available = self.capacity - self.size;
        let to_write = std::cmp::min(data.len(), available);
        
        if to_write > 0 {
            let mut buffer = self.data.lock().await;
            buffer[self.size..self.size + to_write].copy_from_slice(&data[..to_write]);
            self.size += to_write;
        }
        
        Ok(to_write)
    }

    /// Read data from the buffer
    pub async fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let to_read = std::cmp::min(buf.len(), self.size);
        
        if to_read > 0 {
            let buffer = self.data.lock().await;
            buf[..to_read].copy_from_slice(&buffer[..to_read]);
            
            // Shift remaining data
            let mut buffer = self.data.lock().await;
            buffer.copy_within(to_read..self.size, 0);
            self.size -= to_read;
        }
        
        Ok(to_read)
    }

    /// Get the current size
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get the capacity
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Check if buffer is empty
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    /// Check if buffer is full
    pub fn is_full(&self) -> bool {
        self.size == self.capacity
    }

    /// Get available space
    pub fn available_space(&self) -> usize {
        self.capacity - self.size
    }

    /// Clear the buffer
    pub async fn clear(&mut self) {
        self.size = 0;
    }

    /// Get buffer utilization percentage
    pub fn utilization(&self) -> f64 {
        (self.size as f64 / self.capacity as f64) * 100.0
    }
}

impl Default for MemoryMappedBuffer {
    fn default() -> Self {
        Self::default()
    }
}

/// Buffer pool for efficient buffer reuse
pub struct BufferPool {
    buffers: Arc<Mutex<Vec<RingBuffer>>>,
    buffer_size: usize,
    max_pool_size: usize,
}

impl BufferPool {
    /// Create a new buffer pool
    pub fn new(buffer_size: usize, max_pool_size: usize) -> Self {
        Self {
            buffers: Arc::new(Mutex::new(Vec::new())),
            buffer_size,
            max_pool_size,
        }
    }

    /// Create a buffer pool with default settings
    pub fn default() -> Self {
        Self::new(8192, 10)
    }

    /// Get a buffer from the pool
    pub async fn get_buffer(&self) -> RingBuffer {
        let mut buffers = self.buffers.lock().await;
        
        if let Some(mut buffer) = buffers.pop() {
            buffer.clear();
            buffer
        } else {
            RingBuffer::new(self.buffer_size)
        }
    }

    /// Return a buffer to the pool
    pub async fn return_buffer(&self, mut buffer: RingBuffer) {
        let mut buffers = self.buffers.lock().await;
        
        if buffers.len() < self.max_pool_size {
            buffer.clear();
            buffers.push(buffer);
        }
    }

    /// Get pool statistics
    pub async fn statistics(&self) -> PoolStatistics {
        let buffers = self.buffers.lock().await;
        PoolStatistics {
            available_buffers: buffers.len(),
            max_pool_size: self.max_pool_size,
            buffer_size: self.buffer_size,
        }
    }
}

/// Buffer pool statistics
#[derive(Debug, Clone)]
pub struct PoolStatistics {
    /// Number of available buffers
    pub available_buffers: usize,
    /// Maximum pool size
    pub max_pool_size: usize,
    /// Buffer size
    pub buffer_size: usize,
}

impl Default for BufferPool {
    fn default() -> Self {
        Self::default()
    }
}

/// Convenience function to create a ring buffer
pub fn ring_buffer(capacity: usize) -> RingBuffer {
    RingBuffer::new(capacity)
}

/// Convenience function to create a circular buffer
pub fn circular_buffer(initial_capacity: usize) -> CircularBuffer {
    CircularBuffer::new(initial_capacity)
}

/// Convenience function to create a memory-mapped buffer
pub fn memory_mapped_buffer(capacity: usize) -> MemoryMappedBuffer {
    MemoryMappedBuffer::new(capacity)
}

/// Convenience function to create a buffer pool
pub fn buffer_pool(buffer_size: usize, max_pool_size: usize) -> BufferPool {
    BufferPool::new(buffer_size, max_pool_size)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ring_buffer() {
        let mut buffer = RingBuffer::new(100);
        
        // Test write
        let data = b"hello world";
        let written = buffer.write(data).unwrap();
        assert_eq!(written, data.len());
        assert_eq!(buffer.size(), data.len());
        
        // Test read
        let mut read_buf = vec![0; data.len()];
        let read = buffer.read(&mut read_buf).unwrap();
        assert_eq!(read, data.len());
        assert_eq!(&read_buf, data);
        assert_eq!(buffer.size(), 0);
    }

    #[test]
    fn test_ring_buffer_wraparound() {
        let mut buffer = RingBuffer::new(10);
        
        // Fill buffer
        let data1 = b"12345";
        let data2 = b"67890";
        
        buffer.write(data1).unwrap();
        buffer.write(data2).unwrap();
        
        assert_eq!(buffer.size(), 10);
        assert!(buffer.is_full());
        
        // Read some data
        let mut read_buf = vec![0; 5];
        buffer.read(&mut read_buf).unwrap();
        assert_eq!(&read_buf, data1);
        
        // Write more data (should wrap around)
        let data3 = b"abcde";
        let written = buffer.write(data3).unwrap();
        assert_eq!(written, 5);
    }

    #[test]
    fn test_circular_buffer_growth() {
        let mut buffer = CircularBuffer::new(10);
        
        // Write data that exceeds initial capacity
        let data = b"hello world this is a long string";
        let written = buffer.write(data).unwrap();
        assert_eq!(written, data.len());
        assert!(buffer.capacity() > 10);
        
        // Read data back
        let mut read_buf = vec![0; data.len()];
        let read = buffer.read(&mut read_buf).unwrap();
        assert_eq!(read, data.len());
        assert_eq!(&read_buf, data);
    }

    #[tokio::test]
    async fn test_memory_mapped_buffer() {
        let mut buffer = MemoryMappedBuffer::new(100);
        
        // Test write
        let data = b"hello world";
        let written = buffer.write(data).await.unwrap();
        assert_eq!(written, data.len());
        assert_eq!(buffer.size(), data.len());
        
        // Test read
        let mut read_buf = vec![0; data.len()];
        let read = buffer.read(&mut read_buf).await.unwrap();
        assert_eq!(read, data.len());
        assert_eq!(&read_buf, data);
        assert_eq!(buffer.size(), 0);
    }

    #[tokio::test]
    async fn test_buffer_pool() {
        let pool = BufferPool::new(100, 5);
        
        // Get buffer
        let buffer = pool.get_buffer().await;
        assert_eq!(buffer.capacity(), 100);
        
        // Return buffer
        pool.return_buffer(buffer).await;
        
        // Check statistics
        let stats = pool.statistics().await;
        assert_eq!(stats.available_buffers, 1);
        assert_eq!(stats.max_pool_size, 5);
        assert_eq!(stats.buffer_size, 100);
    }

    #[test]
    fn test_convenience_functions() {
        let _ring = ring_buffer(100);
        let _circular = circular_buffer(100);
        let _memory_mapped = memory_mapped_buffer(100);
        let _pool = buffer_pool(100, 5);
        
        // Just verify they compile and create valid instances
        assert!(_ring.is_empty());
        assert!(_circular.is_empty());
        assert!(_memory_mapped.is_empty());
    }
}
