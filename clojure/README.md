# Sudoku Solver in Clojure

## Installation

This project uses Leinigen to build, it is also a good shortcut to run it.
It supports the simple input of a board as a string or a csv file

## Usage

    $ lein run -s 200080300060070084030500209000105408000000000402706000301007040720040060004010003
    $ lein run -csv ../grids/email_example.sk


## Test

    $ lein test  # Some tests still fail because of the issue explained above.

### Bugs

Clojure map function has an unusual behaviour (when compared with Haskell) which changes the type of the collection being returned.

This coupled with the lack of real pattern matching made the translation from Haskell difficult. I used mutimethods, Arity overloading and sequence expanssion (apply) to simulate the same effect. As a result the solver will fail with an java.lang.StackOverflowError for hard to solve sudoku.

A solution will be to replace that with a method that allows me to use the tail recursive construct from clojure (recur)

It still does not support the input as a csv file.

## License

Copyright © 2014 Oscar Forero

Distributed under the MIT License [License](../LICENSE)
