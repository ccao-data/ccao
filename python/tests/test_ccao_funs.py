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
    assessment_group: str,
) -> Tuple[List[str], List[tuple]]:
    """Helper to mock a pyathena Connection and Cursor returning deterministic
    output without making API calls."""
    cols = [
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


def make_mock_read_parquet(
    monkeypatch, succeed_on: str, assessment_group: str
) -> List[str]:
    """
    Create and register a mock for pd.read_parquet that succeeds only on the
    specified path tier. Returns the list of called paths.

    succeed_on: one of "group_folder", "root_md5", "root_flat", "none"
    """
    called_paths: List[str] = []
    expected_folder = (
        "model-condo-avm" if assessment_group == "condo" else "model-res-avm"
    )

    def _fake_read_parquet(
        path: str, *args: Any, **kwargs: Any
    ) -> pd.DataFrame:
        called_paths.append(path)

        is_group_folder = expected_folder in path
        is_root_md5 = not is_group_folder and "/files/md5/" in path
        is_root_flat = not is_group_folder and "/files/md5/" not in path

        ok = {
            "group_folder": is_group_folder,
            "root_md5": is_root_md5,
            "root_flat": is_root_flat,
            "none": False,
        }[succeed_on]

        if not ok:
            raise FileNotFoundError(f"Simulated missing file: {path}")
        return pd.DataFrame({".mock": [True]})

    monkeypatch.setattr(
        "ccao.ccao_funs.pd.read_parquet", _fake_read_parquet, raising=True
    )

    return called_paths


# Parametrized test: correct object type, paths, and fallback behaviour
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "assessment_group,file_keys,succeed_on,expected_paths,expected_total_calls",
    [
        # res: succeeds on group folder (path 1) — 1 attempt per file
        (
            "res",
            ["complex_id", "land_nbhd_rate", "hie"],
            "group_folder",
            [
                "model-res-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "model-res-avm/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                "model-res-avm/files/md5/cc/cccccccccccccccccccccccccccccc",
            ],
            3,
        ),
        # res: falls back to root md5 (path 2) — 2 attempts per file
        (
            "res",
            ["complex_id", "land_nbhd_rate", "hie"],
            "root_md5",
            [
                "model-res-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                "files/md5/cc/cccccccccccccccccccccccccccccc",
            ],
            6,
        ),
        # res: falls back to root (path 3) — 3 attempts per file
        (
            "res",
            ["complex_id", "land_nbhd_rate", "hie"],
            "root_flat",
            [
                "model-res-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                "cc/cccccccccccccccccccccccccccccc",
            ],
            9,
        ),
        # condo: succeeds on group folder (path 1)
        (
            "condo",
            ["complex_id", "land_nbhd_rate", "hie"],
            "group_folder",
            [
                "model-condo-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "model-condo-avm/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                "model-condo-avm/files/md5/cc/cccccccccccccccccccccccccccccc",
            ],
            3,
        ),
        # condo: falls back to root md5 (path 2)
        (
            "condo",
            ["complex_id", "land_nbhd_rate", "hie"],
            "root_md5",
            [
                "model-condo-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                "files/md5/cc/cccccccccccccccccccccccccccccc",
            ],
            6,
        ),
        # condo: falls back to root (path 3)
        (
            "condo",
            ["complex_id", "land_nbhd_rate", "hie"],
            "root_flat",
            [
                "model-condo-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                "bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                "cc/cccccccccccccccccccccccccccccc",
            ],
            9,
        ),
        # single key, res: succeeds on group folder
        (
            "res",
            "complex_id",
            "group_folder",
            ["model-res-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"],
            1,
        ),
        # single key, condo: succeeds on group folder
        (
            "condo",
            "complex_id",
            "group_folder",
            ["model-condo-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"],
            1,
        ),
    ],
    ids=[
        "res_group_folder",
        "res_root_md5_fallback",
        "res_root_flat_fallback",
        "condo_group_folder",
        "condo_root_md5_fallback",
        "condo_root_flat_fallback",
        "res_single_key",
        "condo_single_key",
    ],
)
def test_ccao_download_model_input_data_paths(
    assessment_group,
    file_keys,
    succeed_on,
    expected_paths,
    expected_total_calls,
    monkeypatch,
) -> None:
    mock_cursor_execute(monkeypatch, assessment_group)
    called_paths = make_mock_read_parquet(
        monkeypatch, succeed_on, assessment_group
    )

    data = ccao_download_model_input_data("2025-01-11-gallant-rina", file_keys)

    if isinstance(file_keys, list):
        assert isinstance(data, dict), "expected dict output"
        assert len(data) == len(file_keys), "wrong dict length"
        assert set(data.keys()) == set(file_keys), "wrong dict keys"
    else:
        assert isinstance(data, pd.DataFrame), (
            "expected DataFrame when requesting a single file key"
        )

    assert len(called_paths) == expected_total_calls, (
        f"expected {expected_total_calls} total parquet calls, "
        f"got {len(called_paths)}: {called_paths}"
    )

    for path in expected_paths:
        bucket_path = f"{AWS_S3_DVC_BUCKET}/{path}"
        assert bucket_path in called_paths, (
            f"expected path not attempted: {bucket_path}. "
            f"Called paths were: {called_paths}"
        )


# All paths fail
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "assessment_group",
    ["res", "condo"],
    ids=["res_all_fail", "condo_all_fail"],
)
def test_ccao_download_model_input_data_all_paths_fail(
    assessment_group, monkeypatch
) -> None:
    mock_cursor_execute(monkeypatch, assessment_group)
    called_paths = make_mock_read_parquet(
        monkeypatch, "none", assessment_group
    )

    with pytest.raises(FileNotFoundError, match="Could not find"):
        ccao_download_model_input_data("2025-01-11-gallant-rina", "complex_id")

    # All 3 paths must have been attempted before giving up
    assert len(called_paths) == 3, (
        f"expected 3 path attempts, got {len(called_paths)}: {called_paths}"
    )


# ---------------------------------------------------------------------------
# Error cases
# ---------------------------------------------------------------------------


def test_ccao_download_model_input_data_raises_for_missing_dvc_hash(
    monkeypatch,
) -> None:
    called_paths: List[str] = []

    def _fake_read_parquet(
        path: str, *args: Any, **kwargs: Any
    ) -> pd.DataFrame:
        called_paths.append(path)
        return pd.DataFrame({".mock": [True]})

    monkeypatch.setattr(
        "ccao.ccao_funs.pd.read_parquet", _fake_read_parquet, raising=True
    )

    mock_cursor_execute(monkeypatch, "res")
    run_id = "2025-01-11-gallant-rina"

    with pytest.raises(ValueError) as excinfo:
        ccao_download_model_input_data(run_id, "assessment")

    assert re.search(
        rf"Missing/empty.*run_id\s*=\s*['\"]{re.escape(run_id)}['\"]",
        str(excinfo.value),
        re.IGNORECASE,
    ), f"unexpected missing-hash error message: {excinfo.value}"

    assert len(called_paths) == 0, "parquet was read during missing-hash error"


def test_ccao_download_model_input_data_raises_on_invalid_file_key() -> None:
    called_paths: List[str] = []

    with pytest.raises(ValueError) as excinfo:
        ccao_download_model_input_data(
            "2025-01-11-gallant-rina", "bad_file_key"
        )

    assert re.search(
        r"Invalid file key",
        str(excinfo.value),
        re.IGNORECASE,
    )
    assert len(called_paths) == 0, "parquet was read during invalid-key error"
