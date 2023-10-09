from __future__ import annotations

from asyncio import Queue

import pytest

from medik_pyk import config
from medik_pyk.kompile import MedikSemantics, KompileTarget
from medik_pyk.krun import Medik


def get_configured_semantics():
    medik_semantics = MedikSemantics(
                definition_dir = config.MEDIK_DIR,
                plugin_dir = config.PLUGIN_DIR
            )

    if not config.BUILD_DIR.exists():
        config.BUILD_DIR.mkdir(parents=True, exist_ok=True)

    return medik_semantics


@pytest.mark.asyncio
@pytest.mark.parametrize('build_target', [KompileTarget.LLVM])
async def test_pyk_kompile(build_target: KompileTarget):
    medik_semantics = get_configured_semantics()
    definition_dir = medik_semantics.build(config.BUILD_DIR / build_target.value, target=build_target)
    send_queue = Queue()
    receive_queue = Queue()
    medik = Medik(definition_dir=definition_dir)
    await medik.run(program_file=config.MEDIK_DIR / 'simple-pgm.medik' ,
                    send_queue=send_queue,
                    receive_queue=receive_queue)
