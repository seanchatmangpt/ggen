import pytest
from src.main import Config, main

def test_config_creation():
    config = Config("test", "1.0.0")
    assert config.name == "test"
    assert config.version == "1.0.0"

def test_config_to_dict():
    config = Config("test", "1.0.0")
    config_dict = config.to_dict()
    assert config_dict["name"] == "test"
    assert config_dict["version"] == "1.0.0"

def test_main():
    # Test that main function runs without errors
    try:
        main()
    except Exception as e:
        pytest.fail(f"main() raised {e} unexpectedly!")
