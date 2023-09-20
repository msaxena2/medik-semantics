from argparse import ArgumentParser
from pathlib import Path

from pyk.cli.utils import file_path
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

    kompile_args.add_argument( '-t'
                              , '--target'
                              , type=KompileTarget
                              , help='[llvm|llvm-mcheck|haskell]')

    kompile_args.add_argument( '-o'
                             , '--output-definition'
                             , type=Path
                             , dest='output_dir'
                             , help='Path to write kompiled definition to.'
    )

    return parser

def main():
    sys.setrecursionlimit(15000000)
    parser = create_arg_parser()
    args = parser.parse_args()

    match args.command:
        case 'kompile':
            print(args)
            run_kompile(**vars(args))
        case _:
            raise RuntimeError('{} is not a valid command'.format(str(args.command)))

def run_kompile( output=None
               , target=None
               , **kwargs):



if __name__ == '__main__':
    main()
