import os
import sys

from sphinx_pyproject import SphinxConfig

# Add source path to sys path so that autodoc can load functions
sys.path.append(os.path.abspath("../.."))

# Loads config from pyproject.toml
config = SphinxConfig("../../pyproject.toml", globalns=globals())

# Options that can't be parsed by sphinx-pyproject
html_sidebars = {"**": []}
