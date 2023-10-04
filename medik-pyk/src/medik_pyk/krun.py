from __future__ import annotations

import logging
from typing import TYPE_CHECKING

from pyk.ktool.krun import KRunOutput, _krun

from .build import BuildTarget

if TYPE_CHECKING:
    from pathlib import Path
    from typing import Any, Final

_LOGGER: Final = logging.getLogger(__name__)
_LOG_FORMAT: Final = '%(levelname)s %(asctime)s %(name)s - %(message)s'


def medik_run(
    input_file: Path, target: BuildTarget = BuildTarget.LLVM, output_mode: KRunOutput = KRunOutput.PRETTY, **kwargs: Any
) -> None:
    if not target.path.exists():
        raise RuntimeError(f'Target {target.value} not built. Build target before execution.')



