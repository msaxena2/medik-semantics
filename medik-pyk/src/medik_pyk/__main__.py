from __future__ import annotations

import logging
from argparse import ArgumentParser
from typing import TYPE_CHECKING

from pyk.cli.args import LoggingOptions, DefinitionOptions, KCLIArgs
from collections.abc import Iterable
from pyk.cli.utils import file_path, dir_path
from pyk.kdist import kdist

if TYPE_CHECKING:
    from argparse import Namespace
    from pathlib import Path
    from typing import Final

_LOGGER: Final = logging.getLogger(__name__)
_LOG_FORMAT: Final = '%(levelname)s %(asctime)s %(name)s - %(message)s'


def main() -> None:
    parser = _create_argument_parser()
    args = parser.parse_args()
    print(args)
    logging.basicConfig(level=_loglevel(args), format=_LOG_FORMAT)
    args = parser.parse_args()
    stripped_args = {
        key: val for (key, val) in vars(args).items() if val is not None and not (isinstance(val, Iterable) and not val)
    }
    options = generate_options(stripped_args)

    executor_name = 'exec_' + args.command.lower().replace('-', '_')
    if executor_name not in globals():
        raise AssertionError(f'Unimplemented command: {args.command}')

    execute = globals()[executor_name]
    execute(options)

def _create_argument_parser() -> ArgumentParser:
    parser = ArgumentParser(prog='medik', description='MediK CLI')

    command_parser = parser.add_subparsers(dest='command', required=True)

    k_cli_args = KCLIArgs()

    run_parser = command_parser.add_parser('run', help='Execute MediK Programs', parents=[k_cli_args.logging_args])
    run_parser.add_argument('-d', '--definition_dir', help='Path to kompiled medik directory', type=dir_path)
    run_parser.add_argument('input_file', help='Input MediK File', type=file_path)

    return parser

def generate_options(args: Dict[str, Any]) -> LoggingOptions:
    match args['command']:
        case 'run':
            return RunOptions(args)
        case _:
            raise ValueError(f'Unrecognized command: {command}')

class RunOptions(LoggingOptions, DefinitionOptions):
    input_file: Path

    @staticmethod
    def default() -> dict[str, Any]:
        return {
            'definition_dir': None
        }

def _loglevel(args: Namespace) -> int:
    if args.debug:
        return logging.DEBUG

    if args.verbose:
        return logging.INFO

    return logging.WARNING


def exec_run(options: RunOptions) -> None:
    use_dir = options.definition_dir or kdist.get('medik-semantics.llvm')
    print('will use dir {}'.format(use_dir))

