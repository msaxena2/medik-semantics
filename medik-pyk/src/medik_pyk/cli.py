from __future__ import annotations

import logging
from argparse import ArgumentParser
from typing import TYPE_CHECKING

from pyk.cli.utils import file_path
from pyk.ktool.krun import KRunOutput

from .kompile import BuildTarget
from .krun import medik_run

if TYPE_CHECKING:
    from pathlib import Path
    from typing import Any, Final


_LOGGER: Final = logging.getLogger(__name__)
_LOG_FORMAT: Final = '%(levelname)s %(asctime)s %(name)s - %(message)s'

# ----------------
# Running Programs
# ----------------


def exec_run(
    input_file: Path,
    do_search: bool = False,
    output_mode: KRunOutput = KRunOutput.PRETTY,
    search_pattern: str | None = None,
    depth: int | None = None,
    **kwargs: Any
) -> None:
    if do_search:
        target = BuildTarget.LLVM_MCHECK
    else:
        target = BuildTarget.LLVM

    medik_run(
        input_file=input_file,
        target=target,
        output_mode=output_mode,
        do_search=do_search,
        search_pattern=search_pattern,
        depth=depth,
        **kwargs,
    )


def _build_arg_parser() -> ArgumentParser:
    parser = ArgumentParser(prog='medik')
    command_parser = parser.add_subparsers(dest='command', required=True)
    build_parser = command_parser.add_parser('build', help='build targets')
    build_parser.add_argument('target', help='[llvm|llvm-mcheck|haskell]', type=BuildTarget)

    run_parser = command_parser.add_parser('run', help='run programs')
    run_parser.add_argument('input_file', help='Input MediK File', type=file_path)
    run_parser.add_argument(
        '--output',
        help='Medik Output Format [pretty|kore|json|none]',
        type=KRunOutput,
        default=KRunOutput.PRETTY,
        dest='output_mode',
    )
    run_parser.add_argument('--search', help='Perform a search', action='store_true', dest='do_search')
    run_parser.add_argument('--pattern', help='Pattern to search for', type=str, dest='search_pattern')
    run_parser.add_argument('--depth', help='Execution Depth', type=int)

    return parser


def main() -> None:
    parser = _build_arg_parser()
    args = parser.parse_args()
    match args.command:
        case 'build':
            args.target.do_build()
        case 'run':
            exec_run(args.input_file, do_search=args.do_search, output_mode=args.output_mode)
