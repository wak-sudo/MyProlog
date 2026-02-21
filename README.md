# MyProlog

![GIF](gifs/myProlog.gif)

Implementation of the [Prolog programming language](https://en.wikipedia.org/wiki/Prolog) in OCaml (02/2026) by Wojciech Kieloch. Written as the final project for the Functional Programming 2025/2026 course at the University of Wrocław.

## Build

### Linux

Requirements:
- Dune
- OCaml

For running tests, you will need the ounit2 library.

`opam install ounit2`

Then you can build the whole project by:

`dune build`

If the ounit2 is not seen by the environment even after installing it, type:

`eval $(opam env)`

You can run the CLI with an example program by invoking `make run` in the bin folder.

## Run

If you specify `help` as the first argument of the CLI, you will receive the guide of the program.

## Project's strucutre

    bin/ -> command line interface
    lib/
        mpInterpreter -> main logic
        mpParser -> parser provided by the academy (not my own work)
    test/ -> tests for the project

## Note

You can view Prolog examples in the `bin/exFiles`.
