from __future__ import annotations

import os
from pathlib import Path
from typing import TYPE_CHECKING

import medik_pyk

if TYPE_CHECKING:
    from typing import Final


MODULE_DIR: Final = Path(medik_pyk.__file__).parent
KDIST_DIR: Final = MODULE_DIR / 'kdist'
SEMANTICS_DIR: Final = KDIST_DIR / 'medik-semantics'
