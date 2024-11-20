import pandas as pd
import pytest

import ccao
import ccao.vars_funs


@pytest.mark.parametrize("output_type", ["inplace", "vector"])
def test_vars_rename_input_data_is_dataframe(output_type, chars_sample_athena):
    data = chars_sample_athena.iloc[:, 13:19].copy()
    result = ccao.vars_rename(
        data=data, names_from="athena", names_to="pretty", output_type=output_type
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
def test_vars_rename_input_data_is_list(output_type):
    result = ccao.vars_rename(
        data=["Apartments", "Cathedral Ceiling"],
        names_from="pretty",
        names_to="model",
        output_type=output_type,
    )
    expected = ["char_apts", "char_tp_dsgn"]
    # Output should be the same regardless of the value of `output_type`
    assert result == expected


def test_vars_rename_hie_to_athena(chars_sample_hie):
    data = chars_sample_hie.iloc[:, 1:3].copy()
    result = ccao.vars_rename(
        data=data, names_from="hie", names_to="athena", output_type="vector"
    )
    expected = ["township_code", "card"]
    assert result == expected


def test_vars_rename_unmatched_cols_unchanged():
    # If columns are not present in the dictionary, leave them as-is
    unmatched_colnames = ["foo", "bar", "baz"]
    result = ccao.vars_rename(
        data=unmatched_colnames, names_from="hie", names_to="athena"
    )
    assert result == unmatched_colnames


@pytest.mark.parametrize("output_type", ["vector", ccao.vars_funs.OutputType.VECTOR])
def test_vars_rename_output_type_enum_or_string(output_type, chars_sample_athena):
    # Both the enum and string versions of output_type should work
    result = ccao.vars_rename(
        data=chars_sample_athena.iloc[:, 13:19],
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
    assert result == expected


def test_vars_rename_custom_dictionary():
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


def test_vars_rename_invalid_dictionary_type():
    with pytest.raises(ValueError) as exc:
        ccao.vars_rename(
            data=["1", "2", "3"],
            names_from="sql",
            names_to="char",
            dictionary={"sql": "char"},
        )
    assert "dictionary must be" in str(exc.value)


def test_vars_rename_invalid_dictionary_empty():
    with pytest.raises(ValueError) as exc:
        ccao.vars_rename(
            data=["1", "2", "3"],
            names_from="sql",
            names_to="char",
            dictionary=pd.DataFrame(),
        )
    assert "non-empty" in str(exc.value)


def test_vars_rename_invalid_dictionary_missing_variable_columns():
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
    assert f"starting with {ccao.vars_funs.var_name_prefix}" in str(exc.value)


@pytest.mark.parametrize("names_from,names_to", [(1, "pretty"), ("pretty", 1)])
def test_vars_rename_invalid_names_type(names_from, names_to):
    with pytest.raises(ValueError) as exc:
        ccao.vars_rename(data=["1", "2", "3"], names_from=names_from, names_to=names_to)
    assert "must be strings" in str(exc.value)


@pytest.mark.parametrize("names_from,names_to", [("1", "pretty"), ("pretty", "1")])
def test_vars_rename_invalid_names_value(names_from, names_to):
    with pytest.raises(ValueError) as exc:
        ccao.vars_rename(data=["1", "2", "3"], names_from=names_from, names_to=names_to)
    assert "must be one of" in str(exc.value)


def test_vars_rename_invalid_output_type_type():
    with pytest.raises(ValueError) as exc:
        ccao.vars_rename(
            data=["Apartments", "Cathedral Ceiling"],
            names_from="pretty",
            names_to="model",
            output_type=1,
        )
    assert "output_type must be a string or" in str(exc.value)


def test_vars_rename_invalid_output_type_value():
    with pytest.raises(ValueError) as exc:
        ccao.vars_rename(
            data=["Apartments", "Cathedral Ceiling"],
            names_from="pretty",
            names_to="model",
            output_type="foo",
        )
    assert "output_type must be one of" in str(exc.value)


def test_vars_rename_invalid_data_type():
    with pytest.raises(TypeError) as exc:
        ccao.vars_rename(data=1, names_from="athena", names_to="pretty")
    assert str(exc.value).startswith("data must be")
