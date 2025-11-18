import pathlib

import pandas as pd
import pytest

fixture_dir = pathlib.Path(__file__).parent.parent.parent / "data-raw"


@pytest.fixture(scope="module")
def chars_sample_athena() -> pd.DataFrame:
    """Sample chars with Athena variable names"""
    return pd.read_csv(fixture_dir / "chars_sample_athena.csv")


@pytest.fixture(scope="module")
def chars_sample_hie() -> pd.DataFrame:
    """Sample chars with HIE variable names"""
    return pd.read_csv(fixture_dir / "chars_sample_hie.csv")
