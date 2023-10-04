from __future__ import annotations

import logging
from argparse import ArgumentParser
from typing import TYPE_CHECKING

from pyk.cli.utils import file_path
from pyk.ktool.krun import KRunOutput

from .build import BuildTarget

if TYPE_CHECKING:
    from pathlib import Path
    from typing import Final


_LOGGER: Final = logging.getLogger(__name__)
_LOG_FORMAT: Final = '%(levelname)s %(asctime)s %(name)s - %(message)s'

# ----------------
# Running Programs
# ----------------


def exec_run(input_file: Path, output_mode: KRunOutput = KRunOutput.PRETTY) -> None:
    if not BuildTarget.LLVM.path.exists():
        raise RuntimeError(f'Target {str(BuildTarget.LLVM.value)} does not exist.')


def _build_arg_parser() -> ArgumentParser:
    parser = ArgumentParser(prog='medik')
    command_parser = parser.add_subparsers(dest='command', required=True)
    build_parser = command_parser.add_parser('build', help='build targets')
    build_parser.add_argument('target', help='[llvm|llvm-mcheck|haskell]', type=BuildTarget)

    run_parser = command_parser.add_parser('run', help='run programs')
    run_parser.add_argument('input_file', help='Input MediK File', type=file_path)
    return parser


def main() -> None:
    parser = _build_arg_parser()
    args = parser.parse_args()
    match args.command:
        case 'build':
            args.target.do_build()
        case 'run':
            exec_run(args.input_file)
