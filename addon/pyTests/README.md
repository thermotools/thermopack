# Python side Test-Suite for ThermoPack

* [Running tests](#running-tests)
* [Adding tests](#tools-for-adding-tests)
* [Test modules](#test-modules)

## Running tests
This directory contains test modules to be run with [`pytest`](https://docs.pytest.org/en/8.1.x/), which can be installed with
```bash
pip install pytest
```

To run the entire test suite,
```bash
pytest addon/pyTests/
```

To run a specific test module,
```bash
pytest addon/pyTests/my_module.py
```

To run a specific test function,
```bash
pytest addon/pyTests/my_module.py::my_function
```

## Tools for adding tests
The module `./tools.py` contains various utility methods and variables that can be convenient when writing new tests, such as 
`check_eq(a, b, tol)`, and `ALL_EOS`, which can be convenient to use with `pytest.mark.parametrize`

## Test modules

* `test_issues.py` - Tests to ensure that previously resolved issues 
