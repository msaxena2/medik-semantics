from __future__ import annotations

import logging
import shutil
from argparse import ArgumentParser
from enum import Enum
from typing import TYPE_CHECKING

from . import config
from .kompile import KompileTarget, medik_kompile

if TYPE_CHECKING:
    from collections.abc import Mapping
    from pathlib import Path
    from typing import Any, Final


_LOGGER: Final = logging.getLogger(__name__)
_LOG_FORMAT: Final = '%(levelname)s %(asctime)s %(name)s - %(message)s'

# ---------
# K targets
# ---------


class BuildTarget(Enum):
    LLVM = 'llvm'
    LLVM_MCHECK = 'llvm-mcheck'
    HASKELL = 'haskell'

    @property
    def path(self) -> Path:
        return config.BUILD_DIR / self.value

    def clean(self) -> Path:
        shutil.rmtree(self.path, ignore_errors=True)
        return self.path

    def do_build(self) -> None:
        _LOGGER.info(f'Building target {self.name}: {self.path}')
        config.BUILD_DIR.mkdir(parents=True, exist_ok=True)
        try:
            medik_kompile(output_dir=self.path, **_TARGET_PARAMS[self])
        except RuntimeError:
            self.clean()
            raise


_TARGET_PARAMS: Final[Mapping[BuildTarget, Any]] = {
    BuildTarget.LLVM: {
        'target': KompileTarget.LLVM,
        'main_file': config.MEDIK_DIR / 'medik.md',
        'main_module': 'MEDIK',
        'syntax_module': 'MEDIK-SYNTAX',
    },
    BuildTarget.LLVM_MCHECK: {
        'target': KompileTarget.LLVM,
        'main_file': config.MEDIK_DIR / 'medik.md',
        'main_module': 'MEDIK',
        'syntax_module': 'MEDIK-SYNTAX',
    },
    BuildTarget.HASKELL: {
        'target': KompileTarget.HASKELL,
        'main_file': config.MEDIK_DIR / 'medik.md',
        'main_module': 'MEDIK',
        'syntax_module': 'MEDIK-SYNTAX',
    },
}

def _build_arg_parser() -> ArgParser:
    parser = ArgumentParser(prog='medik-build')
    command_parser = parser.add_subparsers(dest='command', required=True)
    build_parser = command_parser.add_parser('build', help='build targets')
    build_parser.add_argument(
        'targets', metavar='TARGET', nargs='*', type=target, default=targets, help='target to build'
    )
    return parser

def main() -> None:
    parser = _build_arg_parser()


