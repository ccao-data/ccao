import numpy as np
import pandas as pd
import pytest

import ccao
import ccao.vars_funs


class TestVarsRename:
    @pytest.mark.parametrize("output_type", ["inplace", "vector"])
    def test_vars_rename_input_data_is_dataframe(
        self, output_type, chars_sample_athena
    ):
        data = chars_sample_athena.iloc[:, 13:19].copy()
        result = ccao.vars_rename(
            data=data,
            names_from="athena",
            names_to="pretty",
            output_type=output_type,
        )
        expected = [
            "Apartments",
            "Cathedral Ceiling",
            "Attic Finish",
            "Garage 1 Attached",
            "Garage 1 Area Included",
            "Garage 1 Size",
        ]
        if output_type == "inplace":
            assert list(result.columns) == expected
        else:
            assert result == expected

    @pytest.mark.parametrize("output_type", ["inplace", "vector"])
    def test_vars_rename_input_data_is_list(self, output_type):
        result = ccao.vars_rename(
            data=["Apartments", "Cathedral Ceiling"],
            names_from="pretty",
            names_to="model",
            output_type=output_type,
        )
        expected = ["char_apts", "char_tp_dsgn"]
        # Output should be the same regardless of the value of `output_type`
        assert result == expected

    def test_vars_rename_hie_to_athena(self, chars_sample_hie):
        data = chars_sample_hie.iloc[:, 1:3].copy()
        result = ccao.vars_rename(
            data=data,
            names_from="hie",
            names_to="athena",
            output_type="vector",
        )
        expected = ["township_code", "card"]
        assert result == expected

    def test_vars_rename_unmatched_cols_unchanged(self):
        # If columns are not present in the dictionary, leave them as-is
        unmatched_colnames = ["foo", "bar", "baz"]
        result = ccao.vars_rename(
            data=unmatched_colnames, names_from="hie", names_to="athena"
        )
        assert result == unmatched_colnames

    def test_vars_rename_custom_dictionary(self):
        result = ccao.vars_rename(
            data=["1", "2", "3"],
            names_from="foo",
            names_to="bar",
            dictionary=pd.DataFrame(
                {
                    "var_name_foo": ["1", "2", "3"],
                    "var_name_bar": ["char_1", "char_2", "char_3"],
                }
            ),
        )
        expected = ["char_1", "char_2", "char_3"]
        assert result == expected

    def test_vars_rename_invalid_dictionary_empty(self):
        with pytest.raises(ValueError) as exc:
            ccao.vars_rename(
                data=["1", "2", "3"],
                names_from="sql",
                names_to="char",
                dictionary=pd.DataFrame(),
            )
        assert "non-empty" in str(exc.value)

    def test_vars_rename_invalid_dictionary_missing_variable_columns(self):
        with pytest.raises(ValueError) as exc:
            ccao.vars_rename(
                data=["1", "2", "3"],
                names_from="foo",
                names_to="bar",
                dictionary=pd.DataFrame(
                    {
                        "foo": ["1", "2", "3"],
                        "bar": ["char_1", "char_2", "char_3"],
                    }
                ),
            )
        assert f"starting with {ccao.vars_funs.VAR_NAME_PREFIX}" in str(
            exc.value
        )

    @pytest.mark.parametrize(
        "names_from,names_to", [("1", "pretty"), ("pretty", "1")]
    )
    def test_vars_rename_invalid_names(self, names_from, names_to):
        with pytest.raises(ValueError) as exc:
            ccao.vars_rename(
                data=["1", "2", "3"], names_from=names_from, names_to=names_to
            )
        assert "must be one of" in str(exc.value)

    def test_vars_rename_invalid_output_type(self):
        with pytest.raises(ValueError) as exc:
            ccao.vars_rename(
                data=["Apartments", "Cathedral Ceiling"],
                names_from="pretty",
                names_to="model",
                output_type="foo",
            )
        assert "output_type must be one of" in str(exc.value)


class TestVarsRecode:
    @pytest.fixture(scope="class")
    def raw_columns(cls) -> list[dict]:
        """Metadata describing the columns that we use as fixtures for all
        vars_recode tests. Each element of the list is a dict representing a
        column"""
        return [
            {
                # Structure of the input column. We parameterize input data in
                # the `input_data` fixture with one parameter per element of
                # this dict
                "input": {
                    "athena": {"name": "pin", "value": ["12345"] * 4},
                    "iasworld": {"name": "pin", "value": ["12345"] * 4},
                },
                # Structure of the output column. We select the proper column
                # based on key for different types of tests
                "expected": {
                    "short": {"name": "pin", "value": ["12345"] * 4},
                    "long": {"name": "pin", "value": ["12345"] * 4},
                    "code": {"name": "pin", "value": ["12345"] * 4},
                    "factor": {"name": "pin", "value": ["12345"] * 4},
                    # If `value` is True, expect the column value to be
                    # recoded to the "long" format; otherwise, expect the
                    # column value to stay the same as the input value
                    "col": {"name": "pin", "value": False},
                },
            },
            {
                "input": {
                    "athena": {
                        "name": "char_ext_wall",
                        "value": ["1", "2", "0", None],
                    },
                    "iasworld": {
                        "name": "extwall",
                        "value": ["1", "2", "0", None],
                    },
                },
                "expected": {
                    "short": {
                        "name": "char_ext_wall",
                        "value": ["FRAM", "MASR", np.nan, np.nan],
                    },
                    "long": {
                        "name": "char_ext_wall",
                        "value": ["Frame", "Masonry", np.nan, np.nan],
                    },
                    "code": {
                        "name": "char_ext_wall",
                        "value": ["1", "2", np.nan, np.nan],
                    },
                    "factor": {
                        "name": "char_ext_wall",
                        "value": pd.Categorical(
                            ["1", "2", np.nan, np.nan],
                            categories=["1", "2", "3", "4"],
                        ),
                    },
                    "col": {"name": "char_ext_wall", "value": True},
                },
            },
            {
                "input": {
                    "athena": {
                        "name": "char_bsmt",
                        "value": ["1", "3", "4", "5"],
                    },
                    "iasworld": {
                        "name": "bsmt",
                        "value": ["1", "3", "4", "5"],
                    },
                },
                "expected": {
                    "short": {
                        "name": "char_bsmt",
                        "value": ["FL", "PT", "CR", np.nan],
                    },
                    "long": {
                        "name": "char_bsmt",
                        "value": ["Full", "Partial", "Crawl", np.nan],
                    },
                    "code": {
                        "name": "char_bsmt",
                        "value": ["1", "3", "4", np.nan],
                    },
                    "factor": {
                        "name": "char_bsmt",
                        "value": pd.Categorical(
                            ["1", "3", "4", np.nan],
                            categories=["1", "2", "3", "4"],
                        ),
                    },
                    "col": {"name": "char_bsmt", "value": True},
                },
            },
            {
                "input": {
                    "athena": {"name": "value", "value": range(1000, 1004)},
                    "iasworld": {"name": "value", "value": range(1000, 1004)},
                },
                "expected": {
                    "short": {"name": "value", "value": range(1000, 1004)},
                    "long": {"name": "value", "value": range(1000, 1004)},
                    "code": {"name": "value", "value": range(1000, 1004)},
                    "factor": {"name": "value", "value": range(1000, 1004)},
                    "col": {"name": "value", "value": False},
                },
            },
            {
                "input": {
                    "athena": {
                        "name": "char_roof_cnst",
                        "value": ["1", "2", "3", "0"],
                    },
                    "iasworld": {
                        "name": "user13",
                        "value": ["1", "2", "3", "0"],
                    },
                },
                "expected": {
                    "short": {
                        "name": "char_roof_cnst",
                        "value": ["SHAS", "TRGR", "SLTE", np.nan],
                    },
                    "long": {
                        "name": "char_roof_cnst",
                        "value": [
                            "Shingle + Asphalt",
                            "Tar + Gravel",
                            "Slate",
                            np.nan,
                        ],
                    },
                    "code": {
                        "name": "char_roof_cnst",
                        "value": ["1", "2", "3", np.nan],
                    },
                    "factor": {
                        "name": "char_roof_cnst",
                        "value": pd.Categorical(
                            ["1", "2", "3", np.nan],
                            categories=["1", "2", "3", "4", "5", "6"],
                        ),
                    },
                    "col": {"name": "char_roof_cnst", "value": False},
                },
            },
        ]

    @pytest.fixture(params=["athena", "iasworld"])
    def input_data(cls, request, raw_columns):
        input_type = request.param
        return (
            input_type,
            pd.DataFrame(
                {
                    col["input"][input_type]["name"]: col["input"][input_type][
                        "value"
                    ]
                    for col in raw_columns
                }
            ),
        )

    @pytest.mark.parametrize("code_type", ["short", "long", "code"])
    def test_vars_recode_code_type(self, input_data, raw_columns, code_type):
        input_format, input_data = input_data
        expected_output = pd.DataFrame(
            {
                col["expected"][code_type]["name"]: col["expected"][code_type][
                    "value"
                ]
                for col in raw_columns
            }
        )
        # Rename the expected output data so it's consistent with whatever input
        # data we're looking at
        expected_renamed = ccao.vars_rename(
            expected_output, names_from="model", names_to=input_format
        )
        recoded = ccao.vars_recode(
            input_data, code_type=code_type, as_factor=False
        )
        assert recoded.equals(expected_renamed)

    def test_vars_recode_cols(self, input_data, raw_columns):
        input_format, input_data = input_data
        cols = [
            col["expected"]["col"]["name"]
            for col in raw_columns
            if col["expected"]["col"]["value"] is True
        ]
        # Rename the cols so they match the input data schema
        cols = ccao.vars_rename(
            cols, names_from="model", names_to=input_format
        )
        code_type = "short"
        expected_output = pd.DataFrame(
            {
                col["expected"]["col"]["name"]: (
                    col["expected"][code_type]["value"]
                    if col["expected"]["col"]["value"] is True
                    else col["input"]["athena"]["value"]
                )
                for col in raw_columns
            }
        )
        expected_renamed = ccao.vars_rename(
            expected_output, names_from="model", names_to=input_format
        )
        recoded = ccao.vars_recode(
            input_data, cols=cols, code_type=code_type, as_factor=False
        )
        assert recoded.equals(expected_renamed)

    def test_vars_recode_as_factor(self, input_data, raw_columns):
        input_format, input_data = input_data
        expected_output = pd.DataFrame(
            {
                col["expected"]["factor"]["name"]: col["expected"]["factor"][
                    "value"
                ]
                for col in raw_columns
            }
        )
        expected_renamed = ccao.vars_rename(
            expected_output, names_from="model", names_to=input_format
        )
        recoded = ccao.vars_recode(
            input_data, code_type="code", as_factor=True
        )
        assert recoded.equals(expected_renamed)

    def test_vars_recode_raises_on_empty_dictionary(self):
        with pytest.raises(ValueError) as exc:
            ccao.vars_recode(pd.DataFrame(), dictionary=pd.DataFrame())
        assert "non-empty" in str(exc.value)

    def test_vars_recode_raises_on_missing_dictionary_columns(self):
        dictionary = ccao.vars_dict.drop(columns=["var_code"])
        with pytest.raises(ValueError) as exc:
            ccao.vars_recode(pd.DataFrame(), dictionary=dictionary)
        assert "dictionary must contain the following column" in str(exc.value)

    def test_vars_recode_raises_on_missing_var_name_columns(self):
        dictionary = ccao.vars_dict.drop(
            columns=list(ccao.vars_dict.filter(regex="var_name_"))
        )
        with pytest.raises(ValueError) as exc:
            ccao.vars_recode(pd.DataFrame(), dictionary=dictionary)
        assert "dictionary must contain at least one" in str(exc.value)

    def test_vars_recode_raises_on_invalid_code_type(self):
        with pytest.raises(ValueError) as exc:
            ccao.vars_recode(pd.DataFrame(), code_type="foo")
        assert "code_type must be one of" in str(exc.value)
