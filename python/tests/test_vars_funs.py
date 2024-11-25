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
