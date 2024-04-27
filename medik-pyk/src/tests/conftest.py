from pathlib import Path
from typing import Optional

import pytest
from _pytest.config import Notset
from pyk.cli.utils import dir_path, file_path
from pyk.kbuild import KBuild, Project
from pytest import Config, FixtureRequest, Parser, TempPathFactory

from medik_pyk.medik import Medik

from .config import PROJECT_DIR

def pytest_addoption(parser: Parser) -> None:
    parser.addoption(
        '--kbuild-dir',
        dest='kbuild_dir',
        type=dir_path,
        help='Exisiting kbuild cache directory.'
    )


@pytest.fixture(scope='session')
def kbuild_dir(pytestconfig: Config, tmp_path_factory: TempPathFactory) -> Path:
    existing_kbuild_dir = pytestconfig.getoption('kbuild_dir')
    if not existing_kbuild_dir:
        return tmp_path_factory.mktemp('kbuild')
    else:
        assert isinstance(existing_kbuild_dir, Path)
        return existing_kbuild_dir


@pytest.fixture(scope='session')
def kbuild(kbuild_dir: Path) -> KBuild:
    return KBuild(kbuild_dir)

@pytest.fixture(scope='session')
def project() -> Project:
    return Project.load_from_dir(PROJECT_DIR)

@pytest.fixture(scope='session')
def llvm_dir(kbuild: KBuild, project: Project) -> Path:
    return kbuild.kompile(project, 'llvm')

@pytest.fixture(scope='session')
def medik(llvm_dir: Path) -> Medik:
    return Medik(definition_dir=llvm_dir)

