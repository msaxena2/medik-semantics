from __future__ import annotations

from typing import TYPE_CHECKING

from pyk.ktool.krun import KRun, KRunOutput, _krun
from pyk.ktool.kprint import KPrint
from pyk.kore.parser import KoreParser


if TYPE_CHECKING:
    from asyncio import Queue
    from pathlib import Path

class Medik(KRun, KPrint):
    def __init__(
            self,
            definition_dir: Path,
            main_file: Path | None = None
        ) -> None:

        KRun.__init__(
            self,
            definition_dir = definition_dir,
        )
        KPrint.__init__(
            self,
            definition_dir = definition_dir,
        )

    async def run(self,
        program_file: Path,
        send_queue: Queue,
        receive_queue: Queue,
        keep_alive: bool = False,
        depth: int | None = None,
        do_search: bool = False
    ) -> None:

        try:
            process_res = _krun(
                input_file=program_file,
                definition_dir=self.definition_dir,
                output=KRunOutput.KORE,
                depth=0
            )

        except RuntimeError as err:
            raise RuntimeError('Failed to load program') from err

        current_pattern:Pattern = KoreParser(process_res.stdout).pattern()

        while True:
            next_pattern:Pattern = self.run_pattern(
                                            pattern=current_pattern,
                                            depth=1
                                   )
            if (next_pattern == current_pattern):
                break
            current_pattern = next_pattern

        print(self.kore_to_pretty(current_pattern))


