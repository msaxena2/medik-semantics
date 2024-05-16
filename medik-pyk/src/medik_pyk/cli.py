from __future__ import annotations

import logging
from argparse import ArgumentParser
from typing import TYPE_CHECKING

from pyk.cli.utils import file_path
from pyk.utils import check_file_path
from pyk.ktool.krun import KRunOutput

if TYPE_CHECKING:
    from typing import Final


_LOGGER: Final = logging.getLogger(__name__)
_LOG_FORMAT: Final = '%(levelname)s %(asctime)s %(name)s - %(message)s'

def _build_arg_parser() -> ArgumentParser:
    parser = ArgumentParser(prog='medik', description='MediK CLI')

    run_parser = command_parser.add_parser('run', help='run programs')
    run_parser.add_argument('definition_dir', help='Path to definition')
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



def exec_run(options: RunOptions) -> None:
    # mir_file = Path(input_file)
    check_file_path(options.mir_file)

    if options.definition_dir is None:
        raise RuntimeError('Cannot find KMIR LLVM definition, please specify --definition-dir, or KMIR_LLVM_DIR')
    else:
        # llvm_dir = Path(definition_dir)
        check_dir_path(options.definition_dir)

    if options.depth is not None:
        assert options.depth < 0, ValueError(f'Argument "depth" must be non-negative, got: {options.depth}')

    if options.bug_report:
        br = BugReport(options.mir_file.with_suffix('.bug_report.tar'))
        kmir = KMIR(options.definition_dir, bug_report=br)
    else:
        kmir = KMIR(options.definition_dir)

    run(kmir, options)


