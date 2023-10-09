from __future__ import annotations

from typing import TYPE_CHECKING

from pyk.ktool.krun import KRun, KRunOutput, _krun


if TYPE_CHECKING:
    from asyncio import Queue
    from pathlib import Path

class Medik(KRun):
    def __init__(
            self,
            definition_dir: Path,
            main_file: Path | None = None,
            krun_command: str = 'krun'
        ) -> None:

        KRun.__init__(
            self,
            definition_dir = definition_dir,
            command = krun_command,
        )

    async def run(self,
        program_file: Path,
        send_queue: Queue,
        receive_queue: Queue,
        keep_alive: bool = False,
        depth: int | None = None,
        do_search: bool = False
    ):

        try:
            process_res = _krun(
                input_file=program_file,
                definition_dir=self.definition_dir,
                output=KRunOutput.KORE,
                depth=0
            )
            print(process_res)
        except RuntimeError as err:
            raise RuntimeError('Failed to load program') from err


