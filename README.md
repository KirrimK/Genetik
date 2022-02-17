# Genetik

[![Build](https://github.com/KirrimK/Genetik/actions/workflows/build_on_tag.yml/badge.svg)](https://github.com/KirrimK/Genetik/actions/workflows/build_on_tag.yml)

A weird programming language based on genetics

## Principle

Genetik programs work on the following principle:

- The source code of a program is written using the 4 characters representing the bases in DNA: A, T, C, G

- This DNA is then transcribed into RNA. There is a 50% chance that the twin (complementary) strand will be transcribed instead of the normal (provided) one.

- The resulting RNA strand is then split into codons (groups of 3 bases). The codon-identification process starts from the location in the strand where the start codon AUG is found.
There is a 50% chance that the reading process will read the RNA strand in reverse.

- The codons are then evaluated into amino acids, that are mapped to the instructions of the language and executed.

## Instructions of the language

The virtual machine executing the code (named Cell here) reads a strand of amino acids using an instruction pointer and stores information in two stacks.

The following amino acids are mapped to the following instructions:

### Stack manipulation

| Amino acid | Instruction |
| --: | :-- |
| Phe | Push 0 on top of the active stack |
| Leu | Discard the top of the active stack |
| Ile | Switch the active stack and the other stack |
| Val | Swap the two upper elements of active stack |
| Asp | Duplicate the top of the stack |

### Basic math

| Amino acid | Instruction |
| --: | :-- |
| His | Increment the top of the stack by one |
| Gln | Decrement the top of the stack by one |
| Glu | Add the second-most top of active stack to the top of same stack |
| Trp | Substract the second-most top of active stack to the top of same stack |

### Execution flow control

| Amino acid | Instruction |
| --: | :-- |
| Ser | If the top of the active stack is 0, jump to the corresponding Pro |
| Pro | End of corresponding if block |
| Thr | While the top of the stack is not 0, execute following block |
| Ala | End of corresponding while block |
| Tyr | Ignore every amino acid until Met is read |
| Met | If the Cell was ignoring the code, resume normal operation |
| Stop | Stop execution of the program, return success exit code |

### IO

Several modes can be selected via flags in the command-line to change the execution of IO instructions:

#### Default IO mode
Do not allow the program to get any input at all

#### User interaction mode

Allow user-interaction as the program runs.
Blocks the program waiting for user input, and prints as the program runs.

#### Cached input mode

Allow the interpreter to cache the provided stdin (via a pipe | in a unix shell for example) before starting the program and use it during the execution of the program.
If no data is provided, will go into HEREDOC mode before launching the program, waiting for the user to provide data and end input with Ctrl+D on an empty line.
Every print operation is cached and displayed when the program ends

| Amino acid | Instruction |
| --: | :-- |
| Asn | Get a char from stdin, and place its ASCII value on top of the active stack |
| Lys | Print the top of the stack as an ASCII character to stdout |
| Arg | Print the top of the stack as an integer to stdout |

### Other

| Amino acid | Instruction |
| --: | :-- |
| Gly | (unused) |

Look at the following table to look how to get those codons:
[https://en.wikipedia.org/wiki/Codon_tables#Standard_RNA_codon_table]

## Installation

Download the interpreter from the releases:
[https://github.com/KirrimK/Genetik/releases]

### Build from source

To build from source, you will need to install:
```ocaml``` (from your package manager or windows installer),
```dune``` (from the ```opam``` package manager for example).

Install those dependencies, clone this repo, and run ```dune build```.

## How to use

To execute your Genetik programs, run the Genetik interpreter:
- provide the path to your file as a command-line argument to run it
- or run it to access the Genetik REPL

Several flags can be provided to the interpreter to change its execution mode:

| Flags | Mode |
| --: | :-- |
| -d | Run programs in step by step mode (with debug information) |
| -u | User interaction IO mode |
| -i | Cached input IO mode |
| -n | Force the interpreter to run the normal strand |
| -nr | Force the interpreter to run the normal reversed strand |
| -t | Force the interpreter to run the twin strand |
| -tr | Force the interpreter to run the twin reversed strand |

Combinations of those flags are accepted (except the -u and -i flags).
