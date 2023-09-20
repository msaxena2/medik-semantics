from argparse import ArgumentParser
from pyk.ktool.kompile import KompileArgs, LLVMKompile, LLVMKompileType

from .kompile import KompileTarget

import sys

def create_arg_parser():
    parser = ArgumentParser(prog='kevm-pyk')
    command_parser = parser.add_subparsers(dest='command', required=True)

    kompile_args = command_parser.add_parser(
          'kompile'
        , help='Kompile MediK Definition.'
    )

    kompile_args.add_argument(
            dest='target'
          , type=KompileTarget
          , help='[llvm|llvm-mcheck|haskell]'
    )

    #kompile_args.add_argument('main_file', type=file_path, help='Path to file with main module.')
    #kompile_args.add_argument('--target', type=KompileTarget, help='[llvm|haskell|haskell-standalone|foundry]')
    #kompile_args.add_argument(
    #    '-o', '--output-definition', type=Path, dest='output_dir', help='Path to write kompiled definition to.'
    #)

    krun_args = command_parser.add_parser(
          'krun'
        , help='Run MediK Programs.'
    )

    return parser

def main():
    sys.setrecursionlimit(15000000)
    parser = create_arg_parser()
    args = parser.parse_args()

if __name__ == '__main__':
    main()
