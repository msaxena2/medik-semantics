from __future__ import annotations


from argparse import ArgumentParser



def _create_argument_parser(): ArgumentParser
    parser = ArgumentParser(prog="MediK",
                            description="The MediK Language Tools")


    command_parser = parser.add_subparsers(dest='command', required=True, help='Command to execute')

    run_subparser = command_parser.add_parser('run', help='Run a MediK program')

    run_subparser.add_argument(
        'MediK File',
        type=file_path,
        help='Path to .mir file',
    )
    run_subparser.add_argument(
        '--definition-dir',
        type=dir_path,
        help='Path to LLVM definition to use.',
    )





