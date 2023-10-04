from __future__ import annotations

from typing import TYPE_CHECKING

from pyk.ktool.krun import KRunOutput

from .build import BuildTarget

if TYPE_CHECKING:
    from pathlib import Path
    from typing import Any


def medik_run(
    input_file: Path, target: BuildTarget = BuildTarget.LLVM, output_mode: KRunOutput = KRunOutput.PRETTY, **kwargs: Any
) -> None:
    pass
