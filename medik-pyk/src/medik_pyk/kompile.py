from __future__ import annotations

import sys
from enum import Enum
from pathlib import Path
from typing import TYPE_CHECKING

from pyk.ktool.kompile import HaskellKompile, KompileArgs, LLVMKompile

from . import config

if TYPE_CHECKING:
    from collections.abc import Iterable
    from typing import Final

    from pyk.ktool.kompile import Kompile

HOOK_NAMESPACES: Final = ('JSON', 'KRYPTO')


class KompileTarget(Enum):
    LLVM = 'llvm'
    LLVM_MCHECK = 'llvm-mcheck'
    HASKELL = 'haskell'

    @property
    def md_selector(self) -> str:
        match self:
            case self.LLVM:
                return 'k|concrete'
            case self.LLVM_MCHECK:
                return 'k|mcheck'
            case self.HASKELL:
                return 'k|symbolic'
            case _:
                raise AssertionError()


def _llvm_opts() -> list[str]:
    ccopts = ['-g', '-std=c++17']

    plugin_include = config.PLUGIN_DIR / 'plugin-c'
    ccopts += [
        f'{plugin_include}/json.cpp',
    ]

    return ccopts


def medik_kompile(
    target: KompileTarget,
    output_dir: Path,
    main_file: Path,
    main_module: str,
    syntax_module: str,
    includes: Iterable[str] = (),
    ccopts: Iterable[str] = (),
    llvm_library: Path | None = None,
) -> Path:
    if llvm_library is None:
        llvm_library = output_dir / 'llvm-library'

    include_dirs = [Path(include) for include in includes]
    include_dirs += config.INCLUDE_DIRS

    base_args = KompileArgs(
        main_file=main_file,
        main_module=main_module,
        include_dirs=include_dirs,
        md_selector=target.md_selector,
        hook_namespaces=HOOK_NAMESPACES,
    )

    kompile: Kompile

    try:
        match target:
            case KompileTarget.LLVM:
                ccopts = list(ccopts) + _llvm_opts()
                kompile = LLVMKompile(base_args=base_args, ccopts=ccopts)
                return kompile(output_dir=output_dir)
            case KompileTarget.LLVM_MCHECK:
                ccopts = list(ccopts) + _llvm_opts()
                kompile = LLVMKompile(base_args=base_args, ccopts=ccopts, enable_search=True)
                return kompile(output_dir=output_dir)
            case KompileTarget.HASKELL:
                kompile = HaskellKompile(base_args=base_args)

                return kompile(output_dir=output_dir)
            case _:
                raise ValueError(f'Unsupported target: {target.value}')
    except RuntimeError as err:
        sys.stderr.write(f'\nkompile stdout:\n{err.args[1]}\n')
        sys.stderr.write(f'\nkompile stderr:\n{err.args[2]}\n')
        sys.stderr.write(f'\nkompile returncode:\n{err.args[3]}\n')
        sys.stderr.flush()
        raise
