# Functions for translating variables between different data sources
import importlib.resources

import pandas as pd

import ccao.data

# Load the default variable dictionary
_data_path = importlib.resources.files(ccao.data)
vars_dict = pd.read_csv(str(_data_path / "vars_dict.csv"), dtype=str)

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


def vars_recode(
    data: pd.DataFrame,
    cols: list[str] | None = None,
    code_type: str = "long",
    as_factor: bool = True,
    dictionary: pd.DataFrame | None = None,
) -> pd.DataFrame:
    """
    Replace numerically coded variables with human-readable values.

    The system of record stores characteristic values in a numerically encoded
    format. This function can be used to translate those values into a
    human-readable format. For example, EXT_WALL = 2 will become
    EXT_WALL = "Frame + Masonry". Note that the values and their translations
    must be specified via a user-defined dictionary. The default dictionary is
    :data:`vars_dict`.

    Options for ``code_type`` are:

    - ``"long"``, which transforms EXT_WALL = 1 to EXT_WALL = Frame
    - ``"short"``, which transforms EXT_WALL = 1 to EXT_WALL = FRME
    - ``"code"``, which keeps the original values (useful for removing
      improperly coded values, see the note below)

    :param data:
        A pandas DataFrame with columns to have values replaced.
    :type data: pandas.DataFrame

    :param cols:
        A list of column names to be transformed, or ``None`` to select all columns.
    :type cols: list[str]

    :param code_type:
        The recoding type. See description above for options.
    :type code_type: str

    :param as_factor:
        If True, re-encoded values will be returned as categorical variables
        (pandas Categorical).
        If False, re-encoded values will be returned as plain strings.
    :type as_factor: bool

    :param dictionary:
        A pandas DataFrame representing the dictionary used to translate
        encodings.
    :type dictionary: pandas.DataFrame

    :raises ValueError:
        If the dictionary is missing required columns or if invalid input is
        provided.

    :return:
        The input DataFrame with re-encoded values for the specified columns.
    :rtype: pandas.DataFrame

    .. note::
        Values which are in the data but are NOT in the dictionary will be
        converted to NaN.

    :example:

    .. code-block:: python

        import ccao

        sample_data = ccao.sample_athena

        # Defaults to `long` code type
        ccao.vars_recode(data=sample_data)

        # Recode to `short` code type
        ccao.vars_recode(data=sample_data, code_type="short")

        # Recode only specified columns
        ccao.vars_recode(data=sample_data, cols="GAR1_SIZE")
    """
    # Validate the dictionary schema
    dictionary = dictionary if dictionary is not None else vars_dict
    if dictionary.empty:
        raise ValueError("dictionary must be a non-empty pandas DataFrame")

    required_columns = {"var_code", "var_value", "var_value_short"}
    if not required_columns.issubset(dictionary.columns):
        raise ValueError(
            "Input dictionary must contain the following columns: "
            f"{', '.join(required_columns)}"
        )

    # Validate code type and convert it to the enum
    if code_type not in ["short", "long", "code"]:
        raise ValueError("code_type must be one of 'short', 'long', or 'code'")

    # Filter the dictionary for categoricals only and and pivot it longer for
    # easier lookup
    dict_long = dictionary[
        (dictionary["var_type"] == "char")
        & (dictionary["var_data_type"] == "categorical")
    ]
    dict_long = dict_long.melt(
        id_vars=["var_code", "var_value", "var_value_short"],
        value_vars=[
            col for col in dictionary.columns if col.startswith("var_name_")
        ],
        value_name="var_name",
        var_name="var_type",
    )
    dict_long_pkey = ["var_code", "var_value", "var_value_short", "var_name"]
    dict_long = dict_long[dict_long_pkey]
    dict_long = dict_long.drop_duplicates(subset=dict_long_pkey)

    # Map the code type to its internal representation in the dictionary
    values_to = {
        "code": "var_code",
        "long": "var_value",
        "short": "var_value_short",
    }[code_type]

    # Function to apply to each column to remap column values based on the
    # vars dict
    def transform_column(
        col: pd.Series, var_name: str, values_to: str, as_factor: bool
    ) -> pd.Series | pd.Categorical:
        if var_name in dict_long["var_name"].values:
            var_rows = dict_long[dict_long["var_name"] == var_name]
            # Get a dictionary mapping the possible codes to their values.
            # Use `var_code` as the index (keys) for the dictionary, unless
            # we're selecting `var_code`, in which case we can't set it as the
            # index and use it for values
            var_dict = (
                {code: code for code in var_rows["var_code"].tolist()}
                if values_to == "var_code"
                else var_rows.copy().set_index("var_code")[values_to].to_dict()
            )
            if as_factor:
                return pd.Categorical(
                    col.map(var_dict), categories=list(var_dict.values())
                )
            else:
                return col.map(var_dict)
        return col

    # Recode specified columns, or all columns if none were specified
    cols = cols or data.columns
    for var_name in cols:
        if var_name in data.columns:
            data[var_name] = transform_column(
                data[var_name], var_name, values_to, as_factor
            )

    return data
