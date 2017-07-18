# Fuzzing C-Compilers with QuickCheck

This is an experimental tool that uses QuickCheck to generate random C programs. Testing is done via a monadic action that writes the generated C program into a temporary file on disc and invoking an external shell script to act on it. The exit code of the shell script is used to trigger test failures.


## How to Build

- Clone the repository and within the project directory type:

      $ stack setup
      $ stack build

## Usage

- Create test scripts `test1.sh` and `test2.sh` in the current working directory and make them executable. The fuzzer calls these script with the generated C programs. The exit code should indicate success or failure.

- To start the fuzzer, type:

      $ stack exec fuzzer

- The fuzzer supports some commandline options to select a datatype and a test count. See the online help with:

      $ stack exec fuzzer -- -h

- To run at most `100` tests with expressions based on `32bit unsigned integers`:

      $ stack exec fuzzer -- -t uint32 -c 100


## Note

The generated C code assumes that the datatypes `unsigned char`, `unsigned short`, `unsigned int`, and `unsigned long long` have sizes of 8, 16, 32, and 64 bits respectively.
