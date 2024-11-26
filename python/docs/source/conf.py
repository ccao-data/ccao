import pathlib
import sys

from sphinx_pyproject import SphinxConfig

# Add source path to sys path so that autodoc can load functions
rootdir = pathlib.Path(__file__).resolve().parent.parent.parent
sys.path.append(str(rootdir.resolve()))

# Loads config from pyproject.toml
config = SphinxConfig(rootdir / "pyproject.toml", globalns=globals())

# Options that can't be parsed by sphinx-pyproject
html_sidebars = {"**": []}
