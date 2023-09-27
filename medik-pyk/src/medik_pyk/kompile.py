from __future__ import annotations

from enum import Enum
from pathlib import Path
from typing import TYPE_CHECKING

from pyk.ktool.kompile import HaskellKompile, KompileArgs, LLVMKompile, LLVMKompileType

from . import config

if TYPE_CHECKING:
    from collections.abc import Iterable

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

def _llvm_opts() -> Iterable[str]:
    ccopts = ['-g', '-std=c++17']

    ccopts += ['-lssl', '-lcrypto']

    plugin_dir = dist.check_plugin()

    libff_dir = plugin_dir / 'libff'
    ccopts += [f'{libff_dir}/lib/libff.a', f'-I{libff_dir}/include']

    plugin_include = config.PLUGIN_DIR / 'plugin-c'
    ccopts += [
        f'{plugin_include}/json.cpp',
    ]


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
            hook_namespaces=HOOK_NAMESPACES
        )

    try:
        match target:
            case KompileTarget.LLVM:
                ccopts = ccopts + _llvm_opts()
                kompile = KompileOptions(
                                base_args=base_args,
                                ccopts=ccopts,
                          )
                return kompile(output_dir=output_dir, debug=debug, verbose=verbose)
            case _:
                return output_dir






