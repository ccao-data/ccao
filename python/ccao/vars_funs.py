import enum
import importlib.resources

import pandas as pd

import ccao.data

# Load the default variable dictionary
_data_path = importlib.resources.files(ccao.data)
vars_dict = pd.read_csv(str(_data_path / "vars_dict.csv"))
vars_dict_legacy = pd.read_csv(str(_data_path / "vars_dict_legacy.csv"))


class Name(enum.Enum):
    """Valid sources of names for variables."""

    HIE = "hie"
    IASWORLD = "iasworld"
    ATHENA = "athena"
    MODEL = "model"
    PUBLISH = "publish"
    PRETTY = "pretty"

    @classmethod
    def values(cls) -> list[str]:
        """Return all possible values for the enum."""
        return [name.value for name in cls]

    @property
    def colname(self) -> str:
        """Get the dictionary column corresponding to this variable name."""
        return f"var_name_{self.value}"


def vars_rename(
    data: list[str] | pd.DataFrame,
    names_from: Name | str,
    names_to: Name | str,
    inplace: bool = True,
    dictionary: pd.DataFrame | None = None,
) -> list[str] | pd.DataFrame:
    """
    Rename variables from one naming convention to another.

    This function renames columns in a dataset based on a dictionary that maps
    names from one convention to another. It can rename columns in-place or return
    a character vector of renamed columns.

    :param data: DataFrame or list of column names.
        The input data with columns to be renamed. If a DataFrame, renames columns directly.
    :type data: pandas.DataFrame or list[str]

    :param names_from: The source naming convention to rename from.
        Must match a key in the dictionary.
    :type names_from: Name enum or str

    :param names_to: The target naming convention to rename to.
        Must match a key in the dictionary.
    :type names_to: Name enum or str

    :param inplace: Whether to mutate the data in place or return the new names as a list of strings.
        Only used if the input data is a DataFrame.
    :type inplace: str

    :param dictionary: The dictionary for mapping column names.
        Must contain keys like `var_name_<names_from>` and `var_name_<names_to>`.
    :type dictionary: pandas.DataFrame

    :raises ValueError: If required arguments are invalid or the dictionary does not meet format requirements.
    :raises TypeError: If `data` is neither a DataFrame nor a list of column names.

    :return: Either the input data with renamed columns if `inplace is True`
        and the input data is a DataFrame, otherwise a list of renamed columns.
    :rtype: pandas.DataFrame or list[str]

    :example:
        >>> vars_rename(data=sample_data, names_from=Name.MODEL, names_to=Name.ATHENA, inplace=False)
    """
    # Validate the dictionary schema
    dictionary = dictionary or vars_dict
    if not isinstance(dictionary, pd.DataFrame) or len(dictionary) == 0:
        raise ValueError("dictionary must be a non-empty pandas DataFrame")

    if not (isinstance(names_from, Name) or isinstance(names_from, str)) or not (
        isinstance(names_to, Name) or isinstance(names_to, str)
    ):
        raise ValueError("names_from and names_to must be strings or Name instances")

    if isinstance(names_from, str):
        if names_from.lower() not in Name.values():
            raise ValueError(f"names_from must be one of: {Name.values()}")
        names_from = Name[names_from.upper()]

    if isinstance(names_to, str):
        if names_to.lower() not in Name.values():
            raise ValueError(f"names_to must be one of: {Name.values()}")
        names_to = Name[names_to.upper()]

    dictionary_columns = list(dictionary.columns.values)
    if (
        names_from.colname not in dictionary_columns
        or names_to.colname not in dictionary_columns
    ):
        raise ValueError(
            f"{names_from.colname} and {names_to.colname} must be columns in dictionary"
        )

    mapping = dict(zip(dictionary[names_from.colname], dictionary[names_to.colname]))

    if isinstance(data, pd.DataFrame):
        if inplace is True:
            data.rename(columns=mapping, inplace=True)
            return data
        else:
            return [mapping.get(col, col) for col in list(data.columns.values)]
    elif isinstance(data, list):
        return [mapping.get(col, col) for col in data]
    else:
        raise TypeError("data must be a DataFrame or list of column names")
