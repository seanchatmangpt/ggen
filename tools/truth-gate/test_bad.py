from unittest.mock import patch
from unittest.mock import MagicMock

def test_something():
    with patch("ostar.utils.observability"):
        pass
        
    mock = MagicMock()
    return mock
