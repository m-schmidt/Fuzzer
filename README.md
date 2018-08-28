# Fuzzing C-Compilers with QuickCheck

This is an experimental tool that uses QuickCheck to generate random C programs. Testing is done via a monadic action that writes the generated C program into temporary file(s) on disc and invoking an external shell script to act on the program. The exit code of the shell script is used to trigger test failures.


## How to Build

- Clone the repository and within the project directory type:

      $ stack build

## Usage

- Create test scripts `test1.sh` (for expression and loop bound testing modes) and `test2.sh` (for calling conventions mode) in the current working directory and make them executable. The fuzzer calls these scripts with the generated C programs. The exit code of the scripts should indicate success or failure.

- To start the fuzzer, type:

      $ stack exec fuzzer

- The fuzzer supports some commandline options to select a testing mode, the complexity, and test count etc. See the online help with:

      $ stack exec fuzzer -- -h

- To run at most `100` tests with expressions based on `32bit unsigned integers`:

      $ stack exec fuzzer -- -t uint32 -n 100

## Test Modes

- Expressions, `expr`: Generate random expressions on unsigned integer types, evaluate and compare them with a precomputed result.

- Calling Conventions, `conv`: Generate function calls with a random number and randomly typed arguments. The called function (in separate file) checks whether all arguments bits are passed correctlty.

- Loops, `loop`: Generate random loops with known iteration count and loop bound annotations in `ais` format. Can be used to test certain static analyzers.

## Note

The generated C code assumes that the datatypes `unsigned char`, `unsigned short`, `unsigned int`, and `unsigned long long` have sizes of 8, 16, 32, and 64 bits respectively. The size of pointers is assumed to be 32 bits by default. 64 bit pointers can be enabled via the commandline option `--ptr64`.
