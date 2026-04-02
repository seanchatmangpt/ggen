"""Configuration loader for DSPy integration."""

import toml
from pathlib import Path
from typing import Dict, Any


def load_config(config_path: Path = None) -> Dict[str, Any]:
    """
    Load DSPy configuration from TOML file.

    Args:
        config_path: Path to config file (default: dspy_config.toml in current dir)

    Returns:
        Configuration dictionary
    """
    if config_path is None:
        config_path = Path("dspy_config.toml")

    if not config_path.exists():
        raise FileNotFoundError(f"Configuration file not found: {config_path}")

    return toml.load(config_path)
