"""
Tests for ISO-20022 IBAN/BIC validators — spec 035 T003.
No unittest.mock, MagicMock, @patch, or TODO comments.
"""
import pytest

try:
    import schwifty as _schwifty
    SCHWIFTY_AVAILABLE = True
except ImportError:
    SCHWIFTY_AVAILABLE = False

pytestmark = pytest.mark.skipif(
    not SCHWIFTY_AVAILABLE,
    reason="schwifty not installed"
)


def test_validate_iban_invalid():
    """validate_iban returns False for a non-IBAN string (schwifty raises ValueError)."""
    from iso20022 import validate_iban
    assert validate_iban("NOTANIBAN") is False


def test_validate_bic_invalid():
    """validate_bic returns False for a non-BIC string."""
    from iso20022 import validate_bic
    assert validate_bic("NOTABIC") is False


def test_format_iban_fallback():
    """format_iban returns the original string unchanged when it is not a valid IBAN."""
    from iso20022 import format_iban
    result = format_iban("NOTANIBAN")
    assert result == "NOTANIBAN"


def test_keyboard_interrupt_propagates():
    """
    KeyboardInterrupt must NOT be swallowed by validate_iban.

    Uses a plain Python class (no mock library) that raises KeyboardInterrupt
    when called.  The class is temporarily injected into the module's namespace
    so the production code exercises it.
    """
    import iso20022 as _mod

    class _KIRaisingIBAN:
        """Raises KeyboardInterrupt on instantiation — stands in for schwifty.IBAN."""
        def __init__(self, value):
            raise KeyboardInterrupt("simulated interrupt")

    original = _mod.IBAN
    _mod.IBAN = _KIRaisingIBAN
    try:
        with pytest.raises(KeyboardInterrupt):
            _mod.validate_iban("test")
    finally:
        _mod.IBAN = original
