"""
Tests and benchmarks for NotificationHub queue depth handling.
"""

import asyncio
import sys
import os
import time
import unittest

# Ensure src/python is on sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../src/python")))

from notification_hub import NotificationHub, Channel, Message, MessageQueue


class MockQueue:
    def __init__(self, depth_val: int, delay_s: float = 0.002):
        self._depth = depth_val
        self._delay = delay_s

    async def enqueue(self, message: Message) -> None:
        pass

    async def dequeue(self, count: int):
        return []

    async def peek(self, count: int):
        return []

    async def depth(self) -> int:
        if self._delay > 0:
            await asyncio.sleep(self._delay)
        return self._depth

    async def acknowledge(self, message_id: str) -> None:
        pass


class TestNotificationHubQueueDepth(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.hub = NotificationHub()
        self.channels = list(Channel)
        for i, channel in enumerate(self.channels):
            self.hub.register_queue(channel, MockQueue(depth_val=(i + 1) * 10, delay_s=0.002))

    async def test_get_queue_depth_single_channel(self):
        depth = await self.hub.get_queue_depth(Channel.EMAIL)
        self.assertEqual(depth, 10)

    async def test_get_queue_depth_unregistered_channel(self):
        empty_hub = NotificationHub()
        depth = await empty_hub.get_queue_depth(Channel.EMAIL)
        self.assertEqual(depth, 0)

    async def test_get_queue_depth_multiple_channels_list(self):
        if hasattr(self.hub, "get_queue_depths"):
            depths = await self.hub.get_queue_depth([Channel.EMAIL, Channel.SMS])
            self.assertIsInstance(depths, dict)
            self.assertEqual(depths[Channel.EMAIL], 10)
            self.assertEqual(depths[Channel.SMS], 20)

    async def test_get_queue_depths_all_channels(self):
        if hasattr(self.hub, "get_queue_depths"):
            depths = await self.hub.get_queue_depths(chunk_size=3)
            self.assertIsInstance(depths, dict)
            self.assertEqual(len(depths), len(self.channels))
            self.assertEqual(depths[Channel.EMAIL], 10)
            self.assertEqual(depths[Channel.SMS], 20)

    async def test_get_queue_depths_empty_list(self):
        if hasattr(self.hub, "get_queue_depths"):
            depths = await self.hub.get_queue_depths(channels=[])
            self.assertEqual(depths, {})


async def run_benchmark():
    hub = NotificationHub()
    channels = list(Channel)
    for i, ch in enumerate(channels):
        hub.register_queue(ch, MockQueue(depth_val=(i + 1) * 5, delay_s=0.005))

    # Sequential evaluation baseline
    start_seq = time.perf_counter()
    seq_results = {}
    for ch in channels:
        seq_results[ch] = await hub.get_queue_depth(ch)
    elapsed_seq = time.perf_counter() - start_seq

    print(f"Baseline Sequential Time: {elapsed_seq * 1000:.2f} ms")

    if hasattr(hub, "get_queue_depths"):
        start_chunked = time.perf_counter()
        chunked_results = await hub.get_queue_depths(chunk_size=3)
        elapsed_chunked = time.perf_counter() - start_chunked
        print(f"Optimized Chunked Time: {elapsed_chunked * 1000:.2f} ms")
        speedup = elapsed_seq / elapsed_chunked if elapsed_chunked > 0 else float("inf")
        print(f"Speedup: {speedup:.2f}x")

        # Verify exact correctness
        assert seq_results == chunked_results, "Results mismatch!"


if __name__ == "__main__":
    unittest.main(exit=False)
    asyncio.run(run_benchmark())
