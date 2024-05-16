from collections.abc import Mapping
from pathlib import Path
from typing import Any, Final

from pyk.kdist.api import Target

from ..config import SEMANTICS_DIR
from ..kompile import KompileTarget, medik_kompile


class MedikTarget(Target):
    args: dict[str, Any]

    def __init__(self, args: Mapping[str, Any]):
        self.args = dict(args)

    def build(self, output_dir: Path, deps: dict[str, Path], args: dict[str, Any], verbose: bool) -> None:
        enable_llvm_debug = bool(args.get('enable-llvm-debug', ''))
        medik_kompile(output_dir=output_dir, verbose=verbose, **self.args)


_base_options = {
    'main_file': SEMANTICS_DIR / 'medik.md',
    'main_module': 'MEDIK',
    'syntax_module': 'MEDIK-SYNTAX',
}


__TARGETS__: Final = {
    'execution': MedikTarget({
        'target': KompileTarget.EXECUTION,
        } | _base_options),
    'model-check': MedikTarget({
        'target': KompileTarget.MODEL_CHECK,
        } | _base_options)
}

