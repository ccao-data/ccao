# CCAO Python package

This is a Python version of the [`ccao` R
package](https://ccao-data.github.io/ccao/), providing utilities for
managing, distributing, and version controlling *CCAO-specific* functions
used throughout CCAO applications, models, and diagnostics. For generalized
versions of assessment-related functions, see
[assesspy](https://github.com/ccao-data/assesspy).

## Installation

You can install the released version of `ccao` directly from GitHub:

```bash
pip install "git+https://github.com/ccao-data/ccao.git#egg=ccao&subdirectory=python"
```

## Development

Create a development environment using [`uv`](https://docs.astral.sh/uv/):

```
uv venv
source .venv/bin/activate
uv python install
uv pip install .[dev]
```

### Running tests

Run tests with pytest:

```
uv run pytest
```
