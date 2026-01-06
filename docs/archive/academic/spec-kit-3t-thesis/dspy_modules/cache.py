"""LLM output caching with content-based hashing."""

import hashlib
import json
from pathlib import Path
from typing import Any, Optional, Dict
from datetime import datetime, timedelta
import dspy


class DSPyCacheManager:
    """
    Manage DSPy LLM output cache with content-based keys.

    Ensures idempotence: same input → same cached output.
    """

    def __init__(self, cache_dir: Path, ttl_hours: int = 168):
        self.cache_dir = Path(cache_dir)
        self.cache_dir.mkdir(exist_ok=True, parents=True)
        self.ttl = timedelta(hours=ttl_hours)
        self._load_index()

    def _load_index(self) -> None:
        """Load cache index from disk."""
        index_path = self.cache_dir / "index.json"
        if index_path.exists():
            with open(index_path, 'r') as f:
                self.index = json.load(f)
        else:
            self.index = {}

    def _save_index(self) -> None:
        """Persist cache index to disk."""
        index_path = self.cache_dir / "index.json"
        with open(index_path, 'w') as f:
            json.dump(self.index, f, indent=2)

    def _compute_key(
        self,
        module_name: str,
        inputs: Dict[str, Any]
    ) -> str:
        """
        Compute content-based cache key.

        Key = SHA-256(module_name + sorted(inputs))
        """
        # Sort inputs for consistency
        sorted_inputs = json.dumps(inputs, sort_keys=True)
        content = f"{module_name}:{sorted_inputs}"
        return hashlib.sha256(content.encode()).hexdigest()

    def get(
        self,
        module_name: str,
        inputs: Dict[str, Any]
    ) -> Optional[dspy.Prediction]:
        """Retrieve cached prediction if available and not expired."""
        cache_key = self._compute_key(module_name, inputs)

        if cache_key not in self.index:
            return None

        entry = self.index[cache_key]

        # Check TTL
        cached_time = datetime.fromisoformat(entry["timestamp"])
        if datetime.now() - cached_time > self.ttl:
            # Expired - remove from index
            del self.index[cache_key]
            self._save_index()
            return None

        # Load cached prediction
        cache_file = self.cache_dir / f"{cache_key}.json"
        if not cache_file.exists():
            return None

        with open(cache_file, 'r') as f:
            cached_data = json.load(f)

        # Reconstruct dspy.Prediction
        prediction = dspy.Prediction(**cached_data["outputs"])
        return prediction

    def set(
        self,
        module_name: str,
        inputs: Dict[str, Any],
        prediction: dspy.Prediction
    ) -> None:
        """Cache prediction with content-based key."""
        cache_key = self._compute_key(module_name, inputs)

        # Update index
        self.index[cache_key] = {
            "module": module_name,
            "timestamp": datetime.now().isoformat(),
            "input_hash": cache_key
        }
        self._save_index()

        # Save prediction
        cache_file = self.cache_dir / f"{cache_key}.json"
        cache_data = {
            "module": module_name,
            "inputs": inputs,
            "outputs": dict(prediction),
            "timestamp": datetime.now().isoformat()
        }

        with open(cache_file, 'w') as f:
            json.dump(cache_data, f, indent=2)

    def invalidate_all(self) -> int:
        """Clear entire cache. Returns number of entries removed."""
        count = len(self.index)

        # Remove cache files
        for cache_file in self.cache_dir.glob("*.json"):
            if cache_file.name != "index.json":
                cache_file.unlink()

        # Clear index
        self.index = {}
        self._save_index()

        return count

    def stats(self) -> Dict[str, Any]:
        """Return cache statistics."""
        total_entries = len(self.index)
        cache_files = list(self.cache_dir.glob("*.json"))
        total_size_mb = sum(f.stat().st_size for f in cache_files) / (1024 * 1024)

        return {
            "total_entries": total_entries,
            "cache_size_mb": round(total_size_mb, 2),
            "cache_dir": str(self.cache_dir),
            "ttl_hours": self.ttl.total_seconds() / 3600
        }


def cached_dspy_module(cache_manager: DSPyCacheManager):
    """
    Decorator to add caching to DSPy modules.

    Usage:
        @cached_dspy_module(cache_manager)
        class MyModule(dspy.Module):
            ...
    """
    def decorator(module_class):
        original_forward = module_class.forward

        def cached_forward(self, **kwargs):
            module_name = self.__class__.__name__

            # Try cache first
            cached_result = cache_manager.get(module_name, kwargs)
            if cached_result is not None:
                return cached_result

            # Cache miss - run module
            result = original_forward(self, **kwargs)

            # Store in cache
            cache_manager.set(module_name, kwargs, result)

            return result

        module_class.forward = cached_forward
        return module_class

    return decorator
