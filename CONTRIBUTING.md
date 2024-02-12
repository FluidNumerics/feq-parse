# Contributing to feqparse

## Reporting Issues
Issues, including bugs and feature requests, can be reported at https://github.com/FluidNumerics/feq-parse/issues

## Contributing 
If you want to help resolve [any open issues](https://github.com/FluidNumerics/feq-parse/issues), you can do the following :

1. Fork this repository
2. Insert your fixes and commit your changes to your fork. Be sure to include any new additional tests to demonstrate the new feature or bug fix.
3. Open a pull request from your fork to this upstream repository.
4. Work with the upstream reviewer to merge your changes into feq-parse.

When you contribute code, feel add your name to the Contributors section of the README.md

### Code formatting
Each pull request is checked for formatting before running other tests. The `feq-parse` project uses [`fprettify`](https://pypi.org/project/fprettify/) for formatting fortran source code. We have included a configuration file in the `feq-parse` repository (`fprettify.config`) that can be used for ensuring formatting correctness. 

You can run the following to format code to conform to the expected format for `feq-parse`.
```
fprettify  './src/' --config-file ./fprettify.config --recursive --case 1 1 1 1
fprettify  './test/' --config-file ./fprettify.config --recursive --case 1 1 1 1
fprettify  './example/' --config-file ./fprettify.config --recursive --case 1 1 1 1
```
