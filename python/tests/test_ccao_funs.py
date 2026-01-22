# test_ccao_download_model_input_data.py
from __future__ import annotations

import re
from typing import Any, List, Sequence, Tuple

import pandas as pd
import pytest

from ccao.ccao_funs import ccao_download_model_input_data


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


def _mk_metadata_cols_and_rows(
    assessment_year: int,
    assessment_group: str,
) -> Tuple[List[str], List[tuple]]:
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

    row = (
        int(assessment_year),
        str(assessment_group),
        None,  # dvc_md5_assessment_data
        "a" * 32,  # dvc_md5_complex_id_data
        "b" * 32,  # dvc_md5_land_nbhd_rate_data
        None,  # dvc_md5_land_site_rate_data
        None,  # dvc_md5_training_data
        None,  # dvc_md5_char_data
        "c" * 32,  # dvc_md5_hie_data
        None,  # dvc_md5_condo_strata_data
    )

    return cols, [row]


def _run_case(
    monkeypatch: pytest.MonkeyPatch,
    *,
    test_name: str,
    assessment_year: int,
    assessment_group: str,
    expected_path_regexes: Sequence[str],
    run_id: str = "2025-01-11-gallant-rina",
    file_keys: Sequence[str] = ("complex_id", "land_nbhd", "hie"),
) -> None:
    called_paths: List[str] = []

    cols, rows = _mk_metadata_cols_and_rows(assessment_year, assessment_group)

    def _fake_connect(*args: Any, **kwargs: Any) -> _FakeConn:
        return _FakeConn(rows=rows, cols=cols)

    monkeypatch.setattr("ccao.ccao_funs.connect", _fake_connect, raising=True)

    def _fake_read_parquet(
        path: str, *args: Any, **kwargs: Any
    ) -> pd.DataFrame:
        called_paths.append(path)
        return pd.DataFrame({".mock": [True]})

    monkeypatch.setattr(
        "ccao.ccao_funs.pd.read_parquet", _fake_read_parquet, raising=True
    )

    # Call function
    data = ccao_download_model_input_data(run_id, list(file_keys))

    # Basic structure checks
    assert isinstance(data, dict), f"{test_name}: expected dict output"
    assert len(data) == len(file_keys), f"{test_name}: wrong dict length"
    assert set(data.keys()) == set(file_keys), f"{test_name}: wrong dict keys"

    # One parquet read per file
    assert len(called_paths) == len(file_keys), (
        f"{test_name}: wrong number of parquet reads"
    )

    # Path checks: each regex should match at least one called path
    for rx in expected_path_regexes:
        assert any(re.search(rx, p) for p in called_paths), (
            f"{test_name}: no called path matched regex {rx!r}. "
            f"Called paths were: {called_paths}"
        )

    # Invalid file key alone should error and not read any parquet
    called_paths.clear()
    with pytest.raises(ValueError) as excinfo:
        ccao_download_model_input_data(run_id, "bad_file_key")
    assert re.search(
        r"Invalid file key",
        str(excinfo.value),
        re.IGNORECASE,
    )
    assert len(called_paths) == 0, (
        f"{test_name}: parquet was read during invalid-key error"
    )


def test_2025_res_returns_correct_object_and_path(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    _run_case(
        monkeypatch,
        test_name="2025 res returns correct object and path",
        assessment_year=2025,
        assessment_group="res",
        expected_path_regexes=[
            r"/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
            r"/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
            r"/files/md5/cc/cccccccccccccccccccccccccccccc$",
        ],
    )


def test_2026_res_returns_correct_object_and_path(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    _run_case(
        monkeypatch,
        test_name="2026 res returns correct object and path",
        assessment_year=2026,
        assessment_group="res",
        expected_path_regexes=[
            r"model-res-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
            r"model-res-avm/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
            r"model-res-avm/files/md5/cc/cccccccccccccccccccccccccccccc$",
        ],
    )


def test_2026_condo_returns_correct_object_and_path(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    _run_case(
        monkeypatch,
        test_name="2026 condo returns correct object and path",
        assessment_year=2026,
        assessment_group="condo",
        expected_path_regexes=[
            r"model-condo-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
            r"model-condo-avm/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
            r"model-condo-avm/files/md5/cc/cccccccccccccccccccccccccccccc$",
        ],
    )
