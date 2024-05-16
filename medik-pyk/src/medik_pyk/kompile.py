from __future__ import annotations

import concurrent.futures
import logging
import sys
from enum import Enum
from pathlib import Path
from typing import TYPE_CHECKING

from pyk.kdist import kdist
from pyk.ktool.kompile import KompileArgs, LLVMKompile

_LOGGER: Final = logging.getLogger(__name__)

class KompileTarget(Enum):
    EXECUTION = 'execution'
    MODEL_CHECK = 'model-check'
    SYMBOLIC = 'symbolic'

    @property
    def md_selector(self) -> str:
        match self:
            case self.EXECUTION:
                return 'k | concrete '
            case self.MODEL_CHECK:
                return 'k | mcheck'
            case self.SYMBOLIC:
                return 'k | symbolic '
            case _:
                raise AssertionError()


def medik_kompile(
        target: KompileTarget,
        output_dir: Path,
        main_file: Path,
        *,
        main_module: str | None,
        syntax_module: str | None,
        enable_llvm_debug: bool = False,
        debug: bool = False,
        verbose: bool = False
        ) -> Path:

    base_args = KompileArgs(
        main_file=main_file,
        main_module=main_module,
        syntax_module=syntax_module,
        md_selector=target.md_selector,
    )

    match target:
        case KompileTarget.EXECUTION:
            kompile = LLVMKompile(base_args=base_args,
                                  enable_llvm_debug=enable_llvm_debug)
            return kompile(output_dir=output_dir,debug=debug,verbose=verbose)
        case KompileTarget.MODEL_CHECK:
            kompile = LLVMKompile(base_args=base_args,
                                  enable_llvm_debug=enable_llvm_debug,
                                  enable_search=True)
            return kompile(output_dir=output_dir,debug=debug,verbose=verbose)

