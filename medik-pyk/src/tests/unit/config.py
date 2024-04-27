from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import Final


SRC_DIR: Final = Path(__file__).parents[2].resolve(strict=True)
