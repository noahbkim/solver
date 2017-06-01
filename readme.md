# Solver

This repository contains a graph-theory-based grid logic puzzle solver
implemented in Common Lisp. There are several example puzzle files 
included in the repository, all of which should work.

## Puzzles

The puzzle file is flexible in format, and supports comments in the form
of lines prefixed with `#`. There are two sections, the first defining
possible values for the different categories. Items are delimited by 
spaces, and therefore should be one word, but can have any other 
characters.

```
[fields]
category1: example
catgeory2: example
```

These values are referenced in the constraints section, which defines
what constraints are known from the puzzle.

```
[constraints]
# These are associated
category1.example & category2.example
# And these are not
catgeory1.example | category2.example
```

## Runtime

The solver should be executed in a Common Lisp listener, and is called
via `(main "/path/to/puzzle/file.txt")`.
