from __future__ import annotations

import logging
import os
from enum import Enum
from typing import TYPE_CHECKING

from . import config

if TYPE_CHECKING:
    from pathlib import Path
    from typing import Final


_LOGGER: Final = logging.getLogger(__name__)
_LOG_FORMAT: Final = '%(levelname)s %(asctime)s %(name)s - %(message)s'

# ---------
# K targets
# ---------


def _build_dir() -> Path:
    if not os.path.exists(config.build_dir):
        os.mkdir(config.build_dir)
    return config.build_dir


BUILD_DIR: Final = _build_dir()


class BuildTarget(Enum):
    LLVM = 'llvm'
    HASKELL = 'haskell'

    @property
    def path(self) -> Path:
        return BUILD_DIR / self.value
