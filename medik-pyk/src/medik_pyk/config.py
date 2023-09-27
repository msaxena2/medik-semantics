from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import Final

BASE_DIR: Final = Path(__file__).parents[0]

BUILD_DIR: Final = BASE_DIR / '.build'
MEDIK_DIR: Final = BASE_DIR
PLUGIN_DIR: Final = BASE_DIR / 'ext' / 'blockchain-k-plugin'
INCLUDE_DIRS: Final = (MEDIK_DIR, PLUGIN_DIR)
