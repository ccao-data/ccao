# Development

## Setting up a dev environment

Create a development environment using [`uv`](https://docs.astral.sh/uv/):

```
uv venv
source .venv/bin/activate
uv python install
uv pip install .[dev,docs]
```

## Running tests

Run tests with pytest:

```
pytest
```

## Building docs

Build and serve the docs locally with sphinx:

```
sphinx-autobuild docs/source _build/html
```

Navigate to http://localhost:8000 to view the docs.

## Releasing new versions

We handle package releases on GitHub. The `pypi-publish` workflow watches
for release events and pushes new releases to GitHub.

The workflow watches for two types of releases:

- **Pre-release**: If you mark a release as a pre-release in the GitHub UI,
  the workflow will push the package to [the PyPI test
  instance](https://test.pypi.org/).
- **Release**: If you cut a normal release in the GitHub UI, the workflow will
  publish the package to the prod PyPI instance.

Make sure to put up a PR to bump the package version in `pyproject.toml` before
you cut a release.
