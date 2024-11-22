# Functions for translating variables between different data sources
import enum
import importlib.resources

import pandas as pd

import ccao.data

# Load the default variable dictionary
_data_path = importlib.resources.files(ccao.data)
vars_dict = pd.read_csv(str(_data_path / "vars_dict.csv"))

# Prefix we use to identify variable name columns in the variable dictionary
var_name_prefix = "var_name"


class Enum(enum.Enum):
    """Enum subclass with extra convenience functions"""

    @classmethod
    def values(cls) -> list[str]:
        """Get the possible values for this enum"""
        return [type_.value for type_ in cls]


class OutputType(Enum):
    """Possible output types for variable renaming"""

    INPLACE = "inplace"
    VECTOR = "vector"


def _validate_dictionary(
    dictionary: pd.DataFrame | None,
    default: pd.DataFrame
) -> pd.DataFrame:
    """Helper function to validate `dictionary` arguments that are shared among
    variable management functions."""
    # Validate the dictionary schema
    dictionary = dictionary if dictionary is not None else vars_dict
    if not isinstance(dictionary, pd.DataFrame) or len(dictionary) == 0:
        raise ValueError("dictionary must be a non-empty pandas DataFrame")


def vars_rename(
    data: list[str] | pd.DataFrame,
    names_from: str,
    names_to: str,
    output_type: OutputType | str = OutputType.INPLACE,
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

    # Validate input dictionary
    dictionary = _validate_dictionary(dictionary, default=vars_dict)

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


class CodeType(Enum):
    """Possible code formats for variable recoding"""

    LONG = "long"
    SHORT = "short"
    CODE = "code"


def vars_recode(
    data: pd.DataFrame,
    cols: list[str] | None = None,
    code_type: CodeType | str = "long",
    as_factor: bool = True,
    dictionary: pd.DataFrame | None = None
) -> pd.DataFrame:
    """
    Replace numerically coded variables with human-readable values.

    The system of record stores characteristic values in a numerically encoded
    format. This function can be used to translate those values into a
    human-readable format. For example, EXT_WALL = 2 will become
    EXT_WALL = "Frame + Masonry". Note that the values and their translations
    must be specified via a user-defined dictionary. The default dictionary is
    :data:`vars_dict`.

    :param data:
        A pandas DataFrame with columns to have values replaced.
    :type data: pandas.DataFrame

    :param cols:
        A list of column names to be transformed, or None to select all columns.
    :type cols: list[str]

    :param code_type: The recoding type. Options are:
        - "long", which transforms EXT_WALL = 1 to EXT_WALL = Frame
        - "short", which transforms EXT_WALL = 1 to EXT_WALL = FRME
        - "code", which keeps the original values (useful for removing
          improperly coded values).
    :type code_type: CodeType or str

    :param as_factor:
        If True, re-encoded values will be returned as categorical variables
        (pandas Categorical).
        If False, re-encoded values will be returned as plain strings.
    :type as_factor: bool

    :param dictionary:
        A pandas DataFrame representing the dictionary used to translate
        encodings. When None, defaults to :data:`vars_dict`.
    :type dictionary: pandas.DataFrame

    :raises ValueError:
        If the dictionary is missing required columns or if invalid input is
        provided.

    :return:
        The input DataFrame with re-encoded values for the specified columns.
    :rtype: pandas.DataFrame

    :note:
        Values which are in the data but are NOT in the dictionary will be converted to NaN.

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
    # Validate input dictionary
    dictionary =  _validate_dictionary(dictionary, default=vars_dict)

    required_columns = {"var_code", "var_value", "var_value_short"}
    if not required_columns.issubset(dictionary.columns):
        raise ValueError(
            "Input dictionary must contain the following columns: "
            f"{', '.join(required_columns)}"
        )

    # Validate code type and convert it to the enum
    if not isinstance(code_type, (CodeType, str)):
        raise ValueError("code_type must be a string or CodeType instance")

    if isinstance(code_type, str):
        if code_type not in CodeType.values():
            raise ValueError(
                f"code_type must be one of {CodeType.values()} "
                f"(got {code_type})"
            )
        code_type = CodeType(code_type)

    # Validate input data format
    if not isinstance(data, pd.DataFrame):
        raise ValueError("data must be a pandas.DataFrame")

    # Filter and reshape the dictionary for easier lookup
    dict_long = dictionary[
        (dictionary["var_type"] == "char") & (dictionary["var_data_type"] == "categorical")
    ]
    dict_long = dict_long.melt(
        id_vars=[col for col in dict.columns if col.startswith("var_name_")],
        value_vars=["var_code", "var_value", "var_value_short"],
        var_name="var_type",
        value_name="value"
    )
    dict_long = dict_long.drop_duplicates(subset=["var_code", "var_value", "var_value_short", "var_name"])

    # Function to apply transformation on each column
    def transform_column(col: pd.Series, var_name: str) -> pd.Series:
        if var_name in dict_long["var_name"].values:
            var_rows = dict_long[dict_long["var_name"] == var_name]
            idx = col.map(dict_long.set_index("var_code")["value"]).index
            if as_factor:
                return pd.Categorical(col.map(var_rows["value"].get), categories=var_rows["value"])
            else:
                return col.map(var_rows["value"].get)
        return col

    # Recode specified columns or all columns
    cols = cols or data.columns
    for col in cols:
        if col in data.select_dtypes(include=["object", "category"]).columns:
            data[col] = transform_column(data[col], col)

    return data
