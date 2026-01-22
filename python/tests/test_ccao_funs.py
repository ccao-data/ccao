# tests/test_ccao_download_model_input_data.py
import importlib
import re
from typing import List

import pandas as pd
import pytest


class TestCcaoDownloadModelInputData:
    @pytest.fixture(scope="class")
    def run_id(self) -> str:
        return "2025-01-11-gallant-rina"

    @pytest.fixture(scope="class")
    def file_keys(self) -> List[str]:
        return ["complex_id", "land_nbhd", "hie"]

    @pytest.fixture
    def called_paths(self) -> List[str]:
        return []

    @pytest.fixture
    def mock_metadata(self, request) -> pd.DataFrame:
        assessment_year, assessment_group = request.param
        return pd.DataFrame(
            [
                {
                    "assessment_year": int(assessment_year),
                    "assessment_group": str(assessment_group),
                    "dvc_md5_assessment_data": None,
                    "dvc_md5_complex_id_data": "a" * 32,
                    "dvc_md5_land_nbhd_rate_data": "b" * 32,
                    "dvc_md5_land_site_rate_data": None,
                    "dvc_md5_training_data": None,
                    "dvc_md5_char_data": None,
                    "dvc_md5_hie_data": "c" * 32,
                    "dvc_md5_condo_strata_data": None,
                }
            ]
        )

    @pytest.fixture
    def patch_deps(self, monkeypatch, called_paths, mock_metadata):
        """
        Import the module under test and patch its dependencies.
        Adjust module_name to match where your function actually lives.
        """
        module_name = "ccao.ccao_funs"  # <-- file: ccao/ccao_funs.py
        mod = importlib.import_module(module_name)

        class MockAthenaConnection:
            pass

        def mock_dbConnect(*args, **kwargs):
            return MockAthenaConnection()

        def mock_dbDisconnect(*args, **kwargs):
            return None

        def mock_dbGetQuery(*args, **kwargs):
            return mock_metadata.copy()

        def mock_read_parquet(path, *args, **kwargs):
            called_paths.append(path)
            return pd.DataFrame({".mock": [True]})

        # Patch names as they are referenced inside ccao_download_model_input_data
        monkeypatch.setattr(mod, "dbConnect", mock_dbConnect, raising=False)
        monkeypatch.setattr(
            mod, "dbDisconnect", mock_dbDisconnect, raising=False
        )
        monkeypatch.setattr(mod, "dbGetQuery", mock_dbGetQuery, raising=False)

        # Patch parquet readers (depending on your implementation)
        monkeypatch.setattr(
            mod, "read_parquet", mock_read_parquet, raising=False
        )

        # If your module imports pyarrow.parquet as pq
        if hasattr(mod, "pq"):
            monkeypatch.setattr(
                mod.pq,
                "read_table",
                lambda path, **k: mock_read_parquet(path),
                raising=False,
            )

        # If your module imports pyarrow.dataset as ds, etc., patch those similarly.

        return mod

    @pytest.mark.parametrize(
        "mock_metadata,expected_path_regexes",
        [
            pytest.param(
                (2025, "res"),
                [
                    r"/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
                    r"/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
                    r"/files/md5/cc/cccccccccccccccccccccccccccccc$",
                ],
                id="2025-res-legacy",
            ),
            pytest.param(
                (2026, "res"),
                [
                    r"model-res-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
                    r"model-res-avm/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
                    r"model-res-avm/files/md5/cc/cccccccccccccccccccccccccccccc$",
                ],
                id="2026-res",
            ),
            pytest.param(
                (2026, "condo"),
                [
                    r"model-condo-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
                    r"model-condo-avm/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
                    r"model-condo-avm/files/md5/cc/cccccccccccccccccccccccccccccc$",
                ],
                id="2026-condo",
            ),
        ],
        indirect=["mock_metadata"],
    )
    def test_returns_correct_objects_and_paths(
        self,
        patch_deps,
        run_id,
        file_keys,
        called_paths,
        expected_path_regexes,
    ):
        mod = patch_deps

        data = mod.ccao_download_model_input_data(run_id, file_keys)

        assert isinstance(data, dict)
        assert len(data) == len(file_keys)
        assert set(data.keys()) == set(file_keys)

        assert len(called_paths) == len(file_keys)

        for rx in expected_path_regexes:
            assert any(re.search(rx, p) for p in called_paths), (
                f"missing path {rx}"
            )

    def test_invalid_file_key_errors_and_reads_nothing(
        self,
        patch_deps,
        run_id,
        called_paths,
        mock_metadata,
    ):
        mod = patch_deps

        called_paths.clear()
        with pytest.raises(
            Exception,
            match=r"bad_file_key|invalid|Valid keys|possible inputs|Unknown file",
        ):
            mod.ccao_download_model_input_data(run_id, "bad_file_key")

        assert len(called_paths) == 0
