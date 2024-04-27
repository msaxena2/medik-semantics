from __future__ import annotations

from asyncio import Queue

import pytest

from .config import SRC_DIR

@pytest.mark.asyncio
async def test_run(medik:Medik) -> None:
    send_queue: Queue = Queue()
    receive_queue: Queue = Queue()
    await medik.run(program_file=SRC_DIR / 'simple-pgm.medik' ,
                    send_queue=send_queue,
                    receive_queue=receive_queue)
