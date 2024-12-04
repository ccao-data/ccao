================================================
Data dictionary for CCAO data sets and variables
================================================

A crosswalk of CCAO variable names used in iasWorld, AWS, modeling,
and open data. Also includes a translation of numeric character codes
to their human-readable value (ROOF_CNST = 1
becomes ROOF_CNST = Shingle/Asphalt).

Format
------

A pandas DataFrame with the following columns:

- **var_name_hie**: Column name of variable when stored in the legacy ADDCHARS SQL table.
- **var_name_iasworld**: Column name for variable as stored in the system of record (iasWorld).
- **var_name_athena**: Column name used for views and tables in AWS Athena.
- **var_name_model**: Column name used while data is flowing through modeling pipelines.
- **var_name_publish**: Human-readable column name used for public data sets.
- **var_name_pretty**: Human-readable column name used for publication and reporting.
- **var_type**: Variable type/prefix indicating the variable's function. For example,
  ``ind_`` variables are always indicators (booleans), while ``char_`` variables are
  always property characteristics.
- **var_data_type**: R data type variable values should be stored as.
- **var_code**: Factor value for categorical variables. These are the values stored
  in the system of record.
- **var_value**: Human-readable translation of factor value.
- **var_value_short**: Human-readable translation of factor value, but as short as possible.
