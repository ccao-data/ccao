# Functions for translating variables between different data sources
import enum
import importlib.resources
import typing

import pandas as pd

import ccao.data

# Load the default variable dictionary
_data_path = importlib.resources.files(ccao.data)
vars_dict = pd.read_csv(str(_data_path / "vars_dict.csv"))

# Prefix we use to identify variable name columns in the variable dictionary
var_name_prefix = "var_name"


class OutputType(enum.Enum):
    """Possible output types for variable renaming"""

    INPLACE = "inplace"
    VECTOR = "vector"

    @classmethod
    def values(cls) -> list[str]:
        """Get the possible values for this enum"""
        return [type_.value for type_ in cls]


def vars_rename(
    data: typing.Union[list[str], pd.DataFrame],
    names_from: str,
    names_to: str,
    output_type: typing.Union[OutputType, str] = OutputType.INPLACE,
    dictionary: typing.Union[pd.DataFrame, None] = None,
) -> typing.Union[list[str], pd.DataFrame]:
    """
    Rename variables from one naming convention to another.

    This function renames columns in a dataset based on a dictionary that maps
    names from one convention to another. It can rename columns in-place or return
    a character vector of renamed columns, behavior that is configurable using
    the `output_type` argument.

    :param data:
        DataFrame or list of column names to rename.
        If a DataFrame, renames columns directly.
    :type data: pandas.DataFrame or list[str]

    :param names_from:
        The source naming convention to rename from.
        Must match a key in the dictionary.
    :type names_from: str

    :param names_to:
        The target naming convention to rename to.
        Must match a key in the dictionary.
    :type names_to: str

    :param output_type:
        Output type. Either `"inplace"`, which mutates the input data frame,
        or `"vector"`, which returns a list of strings with the construction
        new_col_name = old_col_name.
    :type output_type: OutputType or str

    :param dictionary:
        The dictionary for mapping column names.
        Must contain keys like `var_name_<names_from>` and `var_name_<names_to>`.
    :type dictionary: pandas.DataFrame

    :raises ValueError: If required arguments are invalid or the dictionary does not meet format requirements.
    :raises TypeError: If `data` is neither a DataFrame nor a list of column names.

    :return:
        Either the input data with renamed columns if `output_type` is
        `"inplace"` and the input data is a DataFrame, otherwise a list of
        renamed columns.
    :rtype: pandas.DataFrame or list[str]

    :example:

    .. code-block:: python

        import ccao

        ccao.vars_rename(
            data=["char_yrblt"],
            names_from="athena",
            names_to="pretty",
            output_type="vector"
        )
    """
    # Validate the dictionary schema
    dictionary = dictionary if dictionary is not None else vars_dict
    if not isinstance(dictionary, pd.DataFrame) or len(dictionary) == 0:
        raise ValueError("dictionary must be a non-empty pandas DataFrame")

    # Make sure the dictionary contains variable columns
    dictionary_var_columns = [
        col
        for col in list(dictionary.columns.values)
        if col.startswith(var_name_prefix)
    ]
    if not len(dictionary_var_columns) >= 2:
        raise ValueError(
            f"dictionary must contain at least two columns starting with "
            f"{var_name_prefix}"
        )

    # Get a list of possible names_from and names_to from dictionary
    possible_names_args = [
        col.replace(f"{var_name_prefix}_", "")
        for col in dictionary_var_columns
    ]

    # Validate names arguments
    if not isinstance(names_from, str) or not isinstance(names_to, str):
        raise ValueError("names_from and names_to must be strings")

    # If names arguments aren't possible, throw error and list possible names
    for label, var in [("names_from", names_from), ("names_to", names_to)]:
        if var not in possible_names_args:
            raise ValueError(
                f"{label} must be one of {possible_names_args} (got '{var}')"
            )

    # Validate output type and convert it to the enum
    if not isinstance(output_type, (OutputType, str)):
        raise ValueError("output_type must be a string or OutputType instance")

    if isinstance(output_type, str):
        if output_type not in OutputType.values():
            raise ValueError(
                f"output_type must be one of {OutputType.values()} "
                f"(got {output_type})"
            )
        output_type = OutputType(output_type)

    # Get a mapping from names_from to names_to
    from_ = f"{var_name_prefix}_{names_from}"
    to = f"{var_name_prefix}_{names_to}"
    mapping = dict(zip(dictionary[from_], dictionary[to]))

    # Handle output differently depending on the input and output type args
    if isinstance(data, pd.DataFrame):
        if output_type == OutputType.INPLACE:
            data.rename(columns=mapping, inplace=True)
            return data
        else:
            return [mapping.get(col, col) for col in list(data.columns.values)]
    elif isinstance(data, list):
        # If the input data is a list, it's not possible to update it inplace,
        # so ignore that argument
        return [mapping.get(col, col) for col in data]
    else:
        raise TypeError("data must be a DataFrame or list of column names")
