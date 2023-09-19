from argparse import ArgumentParser
from pyk.ktool.kompile import KompileArgs, LLVMKompile, LLVMKompileType

import sys

def create_arg_parser():
    parser = ArgumentParser(prog='kevm-pyk')
    command_parser = parser.add_subparsers(dest='command', required=True)


def main():
    sys.setrecursionlimit(15000000)
    print('hello poetry')
    parser = create_argument_parser()
    args = parser.parse_args()
    logging.basicConfig(level=_loglevel(args), format=_LOG_FORMAT)

    executor_name = 'exec_' + args.command.lower().replace('-', '_')
    if executor_name not in globals():
        raise AssertionError(f'Unimplemented command: {args.command}')

    execute = globals()[executor_name]
    execute(**vars(args))

if __name__ == '__main__':
    main()
