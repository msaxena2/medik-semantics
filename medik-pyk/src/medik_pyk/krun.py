from __future__ import annotations

from typing import TYPE_CHECKING
from asyncio.queue import Queue

#from pyk.tools.krun import KRun, _krun


if TYPE_CHECKING:
    from typing import final

class Medik(KRun):
    def __init__(
            self,
            definition_dir: Path,
            main_file: Path | None = None,
            krun_command: str = 'krun',
            send_queue: Queue = Queue(),
            receive_queue: Queue = Queue()
        ) -> None:

        KRun.__init__(
            definition_dir = definition_dir,
            main_file = main_file,
            command = krun_command,
        )
        self.send_queue = send_queue
        self.receive_queue = receive_queue

    #async def run(self,
    #    program_file: Path,
    #    keep_alive: bool = False,
    #    depth: int | None = None,
    #    do_search: bool = False,
    #):

    #    try:
    #        proc_res = _krun(
    #            input_file=self.program_file,
    #            definition_dir=self.definition_dir,
    #            output=KRunOutput.KORE,
    #            depth=0,
    #        )
    #    except RuntimeError as err:
    #        raise ReplError('Failed to load program') from err


