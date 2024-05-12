from __future__ import annotations

import logging
from argparse import ArgumentParser
from typing import TYPE_CHECKING

from pyk.cli.args import LoggingOptions
from pyk.cli.utils import file_path
from pyk.utils import check_file_path

if TYPE_CHECKING:
    from argparse import Namespace
    from pathlib import Path
    from typing import Final

_LOGGER: Final = logging.getLogger(__name__)
_LOG_FORMAT: Final = '%(levelname)s %(asctime)s %(name)s - %(message)s'


def main() -> None:
    parser = _create_argument_parser()
    args = parser.parse_args()
    logging.basicConfig(level=_loglevel(args), format=_LOG_FORMAT)

    executor_name = 'exec_' + args.command.lower().replace('-', '_')
    if executor_name not in globals():
        raise AssertionError(f'Unimplemented command: {args.command}')

    execute = globals()[executor_name]
    options = None
    execute(options)


def _create_argument_parser() -> ArgumentParser:
    parser = ArgumentParser(prog='medik', description='MediK CLI')

    command_parser = parser.add_subparsers(dest='command', required=True)

    run_parser = command_parser.add_parser('run', help='run programs')
    run_parser.add_argument('definition_dir', help='Path to definition')
    run_parser.add_argument('input_file', help='Input MediK File', type=file_path)

    return parser


class RunOptions(LoggingOptions):

    def __init__(self, main_directory: Path, main_file: Path | None, use_directory: Path | None) -> None:

        self.main_directory = main_directory
        self.main_file = main_file
        self.use_directory = use_directory


def _loglevel(args: Namespace) -> int:
    if args.debug:
        return logging.DEBUG

    if args.verbose:
        return logging.INFO

    return logging.WARNING


def exec_run(options: RunOptions) -> None:
    if options.main_file is not None:
        check_file_path(options.main_file)
