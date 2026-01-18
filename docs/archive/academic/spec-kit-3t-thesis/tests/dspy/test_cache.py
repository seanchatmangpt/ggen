"""Unit tests for DSPy cache manager."""

import pytest
from pathlib import Path
from datetime import datetime, timedelta
import json


# Skip tests if DSPy not installed
pytest.importorskip("dspy")


@pytest.fixture
def cache_dir(tmp_path):
    """Create temporary cache directory."""
    return tmp_path / "cache"


@pytest.fixture
def cache_manager(cache_dir):
    """Create cache manager instance."""
    from dspy_modules.cache import DSPyCacheManager
    return DSPyCacheManager(cache_dir=cache_dir, ttl_hours=24)


class TestDSPyCacheManager:
    """Tests for DSPyCacheManager."""

    def test_initialization(self, cache_manager, cache_dir):
        """Test cache manager initializes correctly."""
        assert cache_manager.cache_dir == cache_dir
        assert cache_dir.exists()
        assert cache_manager.ttl == timedelta(hours=24)

    def test_compute_key(self, cache_manager):
        """Test content-based key computation."""
        key1 = cache_manager._compute_key(
            "TestModule",
            {"input": "test", "value": 123}
        )
        key2 = cache_manager._compute_key(
            "TestModule",
            {"value": 123, "input": "test"}  # Different order
        )

        # Keys should be identical (sorted inputs)
        assert key1 == key2
        assert len(key1) == 64  # SHA-256 hex digest

    def test_set_and_get(self, cache_manager):
        """Test setting and retrieving cached predictions."""
        import dspy

        # Create mock prediction
        prediction = dspy.Prediction(
            output="Test output",
            score=0.95
        )

        inputs = {"input": "test"}

        # Cache prediction
        cache_manager.set("TestModule", inputs, prediction)

        # Retrieve prediction
        cached = cache_manager.get("TestModule", inputs)

        assert cached is not None
        assert cached.output == "Test output"
        assert cached.score == 0.95

    def test_cache_miss(self, cache_manager):
        """Test cache miss returns None."""
        result = cache_manager.get(
            "NonExistentModule",
            {"input": "test"}
        )
        assert result is None

    def test_ttl_expiration(self, cache_manager, cache_dir):
        """Test cache entries expire after TTL."""
        import dspy

        # Create cache manager with 0.001 hour TTL (3.6 seconds)
        short_ttl_manager = DSPyCacheManager(cache_dir=cache_dir, ttl_hours=0.001)

        prediction = dspy.Prediction(output="Test")
        inputs = {"input": "test"}

        short_ttl_manager.set("TestModule", inputs, prediction)

        # Manually expire entry by modifying timestamp
        index_path = cache_dir / "index.json"
        with open(index_path, 'r') as f:
            index = json.load(f)

        # Set timestamp to 2 hours ago
        for key in index:
            index[key]["timestamp"] = (
                datetime.now() - timedelta(hours=2)
            ).isoformat()

        with open(index_path, 'w') as f:
            json.dump(index, f)

        # Reload index
        short_ttl_manager._load_index()

        # Should return None (expired)
        result = short_ttl_manager.get("TestModule", inputs)
        assert result is None

    def test_invalidate_all(self, cache_manager):
        """Test clearing entire cache."""
        import dspy

        # Add multiple entries
        for i in range(5):
            prediction = dspy.Prediction(output=f"Test {i}")
            cache_manager.set(f"Module{i}", {"input": i}, prediction)

        count = cache_manager.invalidate_all()

        assert count == 5
        assert len(cache_manager.index) == 0

    def test_stats(self, cache_manager):
        """Test cache statistics."""
        import dspy

        # Add entries
        for i in range(3):
            prediction = dspy.Prediction(output=f"Test {i}")
            cache_manager.set(f"Module{i}", {"input": i}, prediction)

        stats = cache_manager.stats()

        assert stats["total_entries"] == 3
        assert stats["cache_size_mb"] >= 0
        assert "cache_dir" in stats
        assert stats["ttl_hours"] == 24


class TestCachedDSPyModule:
    """Tests for cached_dspy_module decorator."""

    def test_decorator_caches_results(self, cache_manager):
        """Test decorator caches module results."""
        from dspy_modules.cache import cached_dspy_module
        import dspy

        call_count = [0]

        @cached_dspy_module(cache_manager)
        class TestModule(dspy.Module):
            def forward(self, input_text: str):
                call_count[0] += 1
                return dspy.Prediction(output=f"Processed: {input_text}")

        module = TestModule()

        # First call - cache miss
        result1 = module(input_text="test")
        assert call_count[0] == 1

        # Second call - cache hit (should not increment count)
        result2 = module(input_text="test")
        assert call_count[0] == 1  # Not incremented

        assert result1.output == result2.output
