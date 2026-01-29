# test_ccao_download_model_input_data.py
from __future__ import annotations

import re
from typing import Any, List, Tuple

import pandas as pd
import pytest

from ccao.ccao_funs import AWS_S3_DVC_BUCKET, ccao_download_model_input_data


class _FakeCursor:
    def __init__(self, rows: List[tuple], cols: List[str]) -> None:
        self._rows = rows
        self._cols = cols

    def __enter__(self) -> "_FakeCursor":
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        return None

    def execute(self, sql: str) -> None:
        return None

    def fetchall(self) -> List[tuple]:
        return self._rows

    @property
    def description(self) -> List[tuple]:
        return [(c, None, None, None, None, None, None) for c in self._cols]


class _FakeConn:
    def __init__(self, rows: List[tuple], cols: List[str]) -> None:
        self._rows = rows
        self._cols = cols

    def cursor(self) -> _FakeCursor:
        return _FakeCursor(self._rows, self._cols)


def mock_cursor_execute(
    monkeypatch: pytest.MonkeyPatch,
    assessment_year: int,
    assessment_group: str,
) -> Tuple[List[str], List[tuple]]:
    """Helper function to mock a pyathena Connection and its Cursor object
    such that it returns deterministic output without making API calls.

    This needs to be a function instead of a fixture because it needs to return
    different input depending on the assessment year/group being requested."""
    cols = [
        "assessment_year",
        "assessment_group",
        "dvc_md5_assessment_data",
        "dvc_md5_complex_id_data",
        "dvc_md5_land_nbhd_rate_data",
        "dvc_md5_land_site_rate_data",
        "dvc_md5_training_data",
        "dvc_md5_char_data",
        "dvc_md5_hie_data",
        "dvc_md5_condo_strata_data",
    ]

    rows = [
        (
            assessment_year,
            assessment_group,
            None,  # dvc_md5_assessment_data (missing hash)
            "a" * 32,  # dvc_md5_complex_id_data
            "b" * 32,  # dvc_md5_land_nbhd_rate_data
            None,  # dvc_md5_land_site_rate_data (missing hash)
            None,  # dvc_md5_training_data (missing hash)
            None,  # dvc_md5_char_data (missing hash)
            "c" * 32,  # dvc_md5_hie_data
            None,  # dvc_md5_condo_strata_data (missing hash)
        )
    ]

    def _fake_connect(*args: Any, **kwargs: Any) -> _FakeConn:
        return _FakeConn(rows=rows, cols=cols)

    monkeypatch.setattr("ccao.ccao_funs.connect", _fake_connect, raising=True)

    return cols, rows


@pytest.fixture
def mock_read_parquet(monkeypatch) -> List[str]:
    """Mock pandas.read_parquet() and return a list storing the arguments used
    to call it"""
    called_paths: List[str] = []

    def _fake_read_parquet(
        path: str, *args: Any, **kwargs: Any
    ) -> pd.DataFrame:
        called_paths.append(path)
        return pd.DataFrame({".mock": [True]})

    monkeypatch.setattr(
        "ccao.ccao_funs.pd.read_parquet", _fake_read_parquet, raising=True
    )

    return called_paths


@pytest.mark.parametrize(
    "assessment_year,assessment_group,file_keys,expected_paths",
    [
        # Single-key, pre-2026, res
        (
            2025,
            "res",
            "complex_id",
            ["files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"],
        ),
        # Multi-key, pre-2026, res
        (
            2025,
            "res",
            ["complex_id", "land_nbhd_rate", "hie"],
            [
                "files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                "files/md5/cc/cccccccccccccccccccccccccccccc",
            ],
        ),
        # Multi-key, post-2026, res
        (
            2026,
            "res",
            ["complex_id", "land_nbhd_rate", "hie"],
            [
                "model-res-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "model-res-avm/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                "model-res-avm/files/md5/cc/cccccccccccccccccccccccccccccc",
            ],
        ),
        # Multi-key, post-2026, condo
        (
            2026,
            "condo",
            ["complex_id", "land_nbhd_rate", "hie"],
            [
                "model-condo-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "model-condo-avm/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                "model-condo-avm/files/md5/cc/cccccccccccccccccccccccccccccc",
            ],
        ),
    ],
    ids=[
        "2025_res_single_key",
        "2025_res_multi_key",
        "2026_res_multi_key",
        "2026_condo_multi_key",
    ],
)
def test_ccao_download_model_input_data_returns_correct_object_and_path(
    assessment_year,
    assessment_group,
    file_keys,
    expected_paths,
    monkeypatch,
    mock_read_parquet,
) -> None:
    mock_cursor_execute(monkeypatch, assessment_year, assessment_group)

    data = ccao_download_model_input_data("2025-01-11-gallant-rina", file_keys)

    if isinstance(file_keys, list):
        assert isinstance(data, dict), "expected dict output"
        assert len(data) == len(file_keys), "wrong dict length"
        assert set(data.keys()) == set(file_keys), "wrong dict keys"
        assert len(mock_read_parquet) == len(file_keys), (
            "wrong number of parquet reads"
        )
    elif isinstance(file_keys, str):
        assert isinstance(data, pd.DataFrame), (
            "expected DataFrame when requesting a single file key"
        )
        assert len(mock_read_parquet) == 1, (
            "expected exactly 1 parquet read for single-key request; "
            f"got {len(mock_read_parquet)}"
        )
    else:
        raise ValueError(
            f"Unexpected input type for file_keys: {type(file_keys)}"
        )

    # Path checks: each regex should match at least one called path
    for path in expected_paths:
        bucket_path = f"{AWS_S3_DVC_BUCKET}/{path}"
        assert bucket_path in mock_read_parquet, (
            f"no called path matched {bucket_path}. "
            f"Called paths were: {mock_read_parquet}"
        )


def test_ccao_download_model_input_data_raises_for_missing_dvc_hash(
    monkeypatch, mock_read_parquet
):
    mock_cursor_execute(monkeypatch, "2025", "res")
    run_id = "2025-01-11-gallant-rina"

    with pytest.raises(ValueError) as excinfo:
        ccao_download_model_input_data(run_id, "assessment")

    assert re.search(
        rf"Missing/empty.*run_id\s*=\s*['\"]{re.escape(run_id)}['\"]",
        str(excinfo.value),
        re.IGNORECASE,
    ), "unexpected missing-hash error message: {excinfo.value}"

    assert len(mock_read_parquet) == 0, (
        "parquet was read during missing-hash error"
    )


def test_ccao_download_model_input_data_raises_on_invalid_file_key(
    mock_read_parquet,
):
    with pytest.raises(ValueError) as excinfo:
        ccao_download_model_input_data(
            "2025-01-11-gallant-rina", "bad_file_key"
        )

    assert re.search(
        r"Invalid file key",
        str(excinfo.value),
        re.IGNORECASE,
    )
    assert len(mock_read_parquet) == 0, (
        "parquet was read during invalid-key error"
    )
