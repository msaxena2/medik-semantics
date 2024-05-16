from __future__ import annotations

from typing import TYPE_CHECKING

from pyk.ktool.krun import KRun, KRunOutput, _krun
from pyk.ktool.kprint import KPrint
from pyk.kore.parser import KoreParser


if TYPE_CHECKING:
    from collections.abc import Iterable
    from asyncio import Queue
    from pathlib import Path
    from pyk.kast.outer import KFlatModule

class Medik(KRun, KPrint):

    def __init__(
                 self,
                 main_directory: Path,
                 main_file: Path | None,
                 use_directory: Path | None,
        ) -> None:

        KRun.__init__(
            self,
            definition_dir = definition_dir,
            use_directory = use_directory
        )
        KPrint.__init__(
            self,
            definition_dir = definition_dir
        )

    async def run(self,
        program_file: Path,
        send_queue: Queue,
        receive_queue: Queue,
        keep_alive: bool = False,
    ) -> None :
        pass



