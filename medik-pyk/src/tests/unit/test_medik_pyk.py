from __future__ import annotations

from typing import TYPE_CHECKING
from asyncio import Queue

import pytest

from medik_pyk import config
from medik_pyk.kompile import MedikSemantics, KompileTarget

def get_configured_semantics():
    medik_semantics = MedikSemantics(
                definition_dir = config.MEDIK_DIR,
                plugin_dir = config.PLUGIN_DIR
            )

    if not config.BUILD_DIR.exists():
        config.BUILD_DIR.mkdir(parents=True, exist_ok=True)

    return medik_semantics


@pytest.mark.parametrize('build_target', [KompileTarget.LLVM])
def test_pyk_kompile(build_target):
    medik_semantics = get_configured_semantics()

    try:
        semantics = medik_semantics.build(config.BUILD_DIR, target=build_target)
        send_queue = asyncio.Queue()
        receive_queue = asyncio.Queue()
        medik = Medik(semantics_dir=semantics)

    except RuntimeError as runtime_error:
        assert False, f'kompile {build_target.value} failed with {runtime_error}'

