from __future__ import annotations

import os
from typing import Dict, List, Union

import pandas as pd
from pyathena import connect

# Bucket that we will use to pull DVC model input data
AWS_S3_DVC_BUCKET = "s3://ccao-data-dvc-us-east-1"


def ccao_download_model_input_data(
    run_id: str,
    file_keys: Union[str, List[str]],
) -> Union[pd.DataFrame, Dict[str, pd.DataFrame]]:
    """
    Download one or more DVC-tracked input datasets for a given model run.

    Args:
        run_id: run_id found in model.metadata.
        file_keys: File key or list of file keys to download. Valid keys are:
            assessment, complex_id, land_nbhd, land_site, training, char,
            hie, condo_strata.

    Returns:
        If `file_keys` is a single key, returns a single DataFrame.
        If `file_keys` is a list, returns a dict of DataFrames keyed by file key.

    # Examples:
    # char_data = ccao_download_model_input_data("2025-01-11-gallant-rina", "char")
    # inputs = ccao_download_model_input_data("2025-01-11-gallant-rina", ["char", "training", "assessment"])

    """
    if isinstance(file_keys, str):
        files_list = [file_keys]
        single = True
    else:
        files_list = list(file_keys)
        single = False

    md5_map: Dict[str, str] = {
        "assessment": "dvc_md5_assessment_data",
        "complex_id": "dvc_md5_complex_id_data",
        "land_nbhd": "dvc_md5_land_nbhd_rate_data",
        "land_site": "dvc_md5_land_site_rate_data",
        "training": "dvc_md5_training_data",
        "char": "dvc_md5_char_data",
        "hie": "dvc_md5_hie_data",
        "condo_strata": "dvc_md5_condo_strata_data",
    }

    valid_files = set(md5_map.keys())
    invalid_files = sorted(set(files_list) - valid_files)
    if invalid_files:
        raise ValueError(
            "Invalid file key(s): "
            + ", ".join(invalid_files)
            + ".\nValid options are: "
            + ", ".join(sorted(valid_files))
            + "."
        )

    # Athena connection
    conn = connect(
        s3_staging_dir=os.getenv(
            "AWS_ATHENA_S3_STAGING_DIR",
            "s3://ccao-athena-results-us-east-1",
        ),
        region_name=os.getenv("AWS_REGION", "us-east-1"),
    )

    sql = f"""
        SELECT
            assessment_year,
            assessment_group,
            dvc_md5_assessment_data,
            dvc_md5_complex_id_data,
            dvc_md5_land_nbhd_rate_data,
            dvc_md5_land_site_rate_data,
            dvc_md5_training_data,
            dvc_md5_char_data,
            dvc_md5_hie_data,
            dvc_md5_condo_strata_data
        FROM model.metadata
        WHERE run_id = '{run_id}'
    """

    with conn.cursor() as cur:
        cur.execute(sql)
        rows = cur.fetchall()
        cols = [d[0] for d in cur.description]

    dvc_params = pd.DataFrame(rows, columns=cols)

    if dvc_params.empty:
        raise ValueError(
            f"No rows found in model.metadata for run_id = '{run_id}'"
        )

    row = dvc_params.iloc[0]
    yr = int(row["assessment_year"])
    grp = str(row["assessment_group"])

    # Folder logic:
    # - <= 2025: old layout
    # - >= 2026: split by assessment_group
    if yr <= 2025:
        model_folder = ""
    elif grp == "condo":
        model_folder = "model-condo-avm"
    else:
        model_folder = "model-res-avm"

    def _read_file(file_key: str) -> pd.DataFrame:
        md5_col = md5_map[file_key]
        dvc_hash = row[md5_col]

        if pd.isna(dvc_hash) or str(dvc_hash).strip() == "":
            raise ValueError(
                f"Missing/empty {md5_col} for run_id = '{run_id}'"
            )

        dvc_hash = str(dvc_hash).strip()
        s3_path = (
            f"{AWS_S3_DVC_BUCKET}"
            + (f"/{model_folder}" if model_folder else "")
            + f"/files/md5/{dvc_hash[:2]}/{dvc_hash[2:32]}"
        )

        return pd.read_parquet(s3_path, engine="pyarrow")

    out: Dict[str, pd.DataFrame] = {k: _read_file(k) for k in files_list}
    return out[files_list[0]] if single else out
