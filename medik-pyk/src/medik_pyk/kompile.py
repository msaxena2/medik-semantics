from __future__ import annotations

import sys
from enum import Enum
from pathlib import Path
from typing import TYPE_CHECKING

from pyk.ktool.kompile import HaskellKompile, KompileArgs, LLVMKompile

if TYPE_CHECKING:
    from pyk.ktool.kompile import Kompile
    from collections.abc import Iterable
    from typing import Final

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
                raise RuntimeError(f'Unrecognized Kompile Target {self.value}')


class MedikSemantics:

    def __init__(
            self,
            definition_dir: Path,
            plugin_dir: Path
        ) -> None:

        print('initializing semantics')
        self.definition_dir = definition_dir
        self.plugin_dir = plugin_dir

    def _llvm_opts(self) -> list[str]:
        ccopts = ['-g', '-std=c++17']
        return ccopts

    def build(
            self,
            output_dir: Path,
            main_file: Path | None = None,
            main_module: str = 'MEDIK',
            syntax_module: str = 'MEDIK-SYNTAX',
            target: KompileTarget = KompileTarget.LLVM,
            includes: Iterable[str] = (),
            ccopts: Iterable[str] = (),
            llvm_library: Path | None = None,
    ) -> Path:

        if main_file == None:
            main_file_path = self.definition_dir / 'medik.md'

        include_dirs = [Path(include) for include in includes]
        include_dirs += (self.definition_dir, self.plugin_dir)

        base_args = KompileArgs(
            main_file=main_file_path,
            main_module=main_module,
            include_dirs=include_dirs,
            md_selector=target.md_selector,
            hook_namespaces=HOOK_NAMESPACES,
        )

        kompile: Kompile

        try:
            match target:
                case KompileTarget.LLVM:
                    ccopts = list(ccopts) + self._llvm_opts()
                    kompile = LLVMKompile(base_args=base_args, ccopts=ccopts)
                    return kompile(output_dir=output_dir)
                case KompileTarget.LLVM_MCHECK:
                    ccopts = list(ccopts) + self._llvm_opts()
                    kompile = LLVMKompile(base_args=base_args, ccopts=ccopts, enable_search=True)
                    return kompile(output_dir=output_dir)
                case KompileTarget.HASKELL:
                    kompile = HaskellKompile(base_args=base_args)

                    return kompile(output_dir=output_dir)
                case _:
                    raise ValueError(f'Unsupported target: {target.value}')
        except RuntimeError as err:
            if len(err.args) > 1:
                sys.stderr.write(f'\nkompile error:\n{err.args[0]}\n')
                sys.stderr.flush()
            raise

