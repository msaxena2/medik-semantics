from __future__ import annotations

import logging
from argparse import ArgumentParser
from typing import TYPE_CHECKING

from pyk.cli.utils import file_path
from pyk.ktool.krun import KRunOutput

from .kompile import KompileTarget

if TYPE_CHECKING:
    from typing import Final


_LOGGER: Final = logging.getLogger(__name__)
_LOG_FORMAT: Final = '%(levelname)s %(asctime)s %(name)s - %(message)s'

def _build_arg_parser() -> ArgumentParser:
    parser = ArgumentParser(prog='medik')
    command_parser = parser.add_subparsers(dest='command', required=True)
    build_parser = command_parser.add_parser('build', help='build targets')
    build_parser.add_argument('target', help='[llvm|llvm-mcheck|haskell]', type=KompileTarget)

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
