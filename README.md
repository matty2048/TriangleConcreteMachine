<!-- omit in toc -->
# Triangle Concrete Machine
<!-- omit in toc -->
# Table of Contents
- [Getting this project](#getting-this-project)
- [Building and testing this project](#building-and-testing-this-project)
  - [REPL](#repl)
- [Project overview](#project-overview)
- [Change the license](#change-the-license)

# Getting this project
```git clone https://github.com/matty2048/TriangleConcreteMachine```

# Building and testing this project
Build the project with:

```bash
stack build
```
To compile the project to Verilog, run:

```bash
stack run clash -- Example.Project --verilog
```

You can find the HDL files in `verilog/`. The source can be found in `src`.

## REPL
Clash offers a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) as a quick way to try things, similar to Python's `python` or Ruby's `irb`. Stack users can open the REPL by invoking:

```
stack run clashi -- "-interactive-print=Text.Pretty.Simple.pPrint" -package pretty-simple
```

# project-overview

The purpose of this project is to go from a high-level-language -> Assembly -> Execution on a CPU all within haskell.

It is broken up into 3 parts
- Compiler
  - this is found in `src/Compiler`
  - This is from university compilers module
- Assembler/linker
  - this is found in `src/Compiler/TAMmt.hs`
  - This replaces string labels with memory adresses and converts abstract machine instructions into actual machine instructions
- CPU
  - this is found in `src/Example` and `src/Modules`
  - executes the instructions provided in `src/Modules/Memory.hs`
