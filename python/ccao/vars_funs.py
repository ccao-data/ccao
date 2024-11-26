# Functions for translating variables between different data sources
import importlib.resources

import pandas as pd

import ccao.data

# Load the default variable dictionary
_data_path = importlib.resources.files(ccao.data)
vars_dict = pd.read_csv(str(_data_path / "vars_dict.csv"))

# Prefix we use to identify variable name columns in the variable dictionary
VAR_NAME_PREFIX = "var_name"


def vars_rename(
    data: list[str] | pd.DataFrame,
    names_from: str,
    names_to: str,
    output_type: str = "inplace",
    dictionary: pd.DataFrame | None = None,
) -> list[str] | pd.DataFrame:
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
        Output type. Either ``"inplace"``, which mutates the input data frame,
        or ``"vector"``, which returns a list of strings with the construction
        new_col_name = old_col_name.
    :type output_type: str

    :param dictionary:
        The dictionary for mapping column names.
        Must contain keys like ``var_name_<names_from>`` and ``var_name_<names_to>``.
    :type dictionary: pandas.DataFrame

    :raises ValueError: If required arguments are invalid or the dictionary does not meet format requirements.
    :raises TypeError: If ``data`` is neither a DataFrame nor a list of column names.

    :return:
        Either the input data with renamed columns if ``output_type`` is
        ``"inplace"`` and the input data is a DataFrame, otherwise a list of
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
    if dictionary.empty:
        raise ValueError("dictionary must be a non-empty pandas DataFrame")

    # Make sure the dictionary contains variable columns
    dictionary_var_columns = [
        col
        for col in list(dictionary.columns.values)
        if col.startswith(VAR_NAME_PREFIX)
    ]
    if not len(dictionary_var_columns) >= 2:
        raise ValueError(
            f"dictionary must contain at least two columns starting with "
            f"{VAR_NAME_PREFIX}"
        )

    # Get a list of possible names_from and names_to from dictionary
    possible_names_args = [
        col.replace(f"{VAR_NAME_PREFIX}_", "")
        for col in dictionary_var_columns
    ]

    # If names arguments aren't possible, throw error and list possible names
    for label, var in [("names_from", names_from), ("names_to", names_to)]:
        if var not in possible_names_args:
            raise ValueError(
                f"{label} must be one of {possible_names_args} (got '{var}')"
            )

    # Validate output type
    if output_type not in ["inplace", "vector"]:
        raise ValueError("output_type must be one of 'inplace' or 'vector'")

    # Get a mapping from names_from to names_to
    from_ = f"{VAR_NAME_PREFIX}_{names_from}"
    to = f"{VAR_NAME_PREFIX}_{names_to}"
    mapping = dict(zip(dictionary[from_], dictionary[to]))

    # Handle output differently depending on the input and output type args
    if isinstance(data, pd.DataFrame):
        if output_type == "inplace":
            data.rename(columns=mapping, inplace=True)
            return data
        else:
            return [mapping.get(col, col) for col in list(data.columns.values)]
    else:
        # If the input data is a list, it's not possible to update it inplace,
        # so ignore that argument
        return [mapping.get(col, col) for col in data]
