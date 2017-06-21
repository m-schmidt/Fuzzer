# Fuzzing C-Compilers with QuickCheck

This is an experimental tool that uses QuickCheck to generate random C programs. Testing is done via a monadic action that writes the generated C program into a temporary file on disc and invoking an external shell script to act on it. The exit code of the shell script is used to trigger test failures.


## How to Build

- Clone the repository and within the project directory type:

      $ stack setup
      $ stack build

## Usage

- Fill `test.sh` with your code to test the generated C program.

- To start the fuzzer, type:

      $ stack exec fuzzer
