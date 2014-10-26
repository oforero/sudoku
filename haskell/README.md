Haskell Sudoku
==============

The following are the steps required to build the project:

  $ cabal install --enable-tests --only-dependencies
  $ cabal configure --enable-tests
  $ cabal build

To run the test suite:

  $ cabal test
  
The test suite have to tests, one expecting solution for all 50 sudokus from Project Euler; and the other expecting a failure for 5 unsolvable sudokus (The first five Project Euler's sudokus modified to be unsolvable).   

The executable program will be in the directory (dist/build/sudoku/), you have to either put that directory in the path or type the path to execute it.

The program accepts a file of rows separated by commas, by using the "-csv" option, it also provides different verbosity levels for the output:

    $ sudoku -csv <input.csv>     # It prints the answer in CSV format 
      1,3,5,2,9,7,8,6,4
      9,8,2,4,1,6,7,5,3
      7,6,4,3,8,5,1,9,2
      2,1,8,7,3,9,6,4,5
      5,9,7,8,6,4,2,3,1
      6,4,3,1,5,2,9,7,8
      4,2,6,5,7,1,3,8,9
      3,5,9,6,2,8,4,1,7
      8,7,1,9,4,3,5,2,6
    $ sudoku -s -csv <input.csv>  # Silent output, only prints Solved | Unsolved
      Solved
    $ sudoku -p -csv <input.csv>  # It pretty prints the output
      ‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒
       •   3   5  |  2   9   •  |  8   6   4
       •   8   2  |  4   1   •  |  7   •   3
       7   6   4  |  3   8   •  |  •   9   •
      ‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒
       2   1   8  |  7   3   9  |  •   4   •
       •   •   •  |  8   •   4  |  2   3   •
       •   4   3  |  •   5   2  |  9   7   •
      ‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒
       4   •   6  |  5   7   1  |  •   •   9
       3   5   9  |  •   2   8  |  4   1   7
       8   •   •  |  9   •   •  |  5   2   6
      ‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒
      
      ‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒
       1   3   5  |  2   9   7  |  8   6   4
       9   8   2  |  4   1   6  |  7   5   3
       7   6   4  |  3   8   5  |  1   9   2
      ‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒
       2   1   8  |  7   3   9  |  6   4   5
       5   9   7  |  8   6   4  |  2   3   1
       6   4   3  |  1   5   2  |  9   7   8
      ‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒
       4   2   6  |  5   7   1  |  3   8   9
       3   5   9  |  6   2   8  |  4   1   7
       8   7   1  |  9   4   3  |  5   2   6
      ‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒

The program also accepts the input as a plain string in the command line (which can be used for scripting), and as file containing a list of puzzles with each one in is own line as string of 81 digits.

    $ sudoku -b 003020600900305001001806400008102900700000008006708200002609500800203009005010300     
    # By default it pretty prints 
    $ sudoku -f <file.sk>

Each of them aceept the "-s" option as a first parameter. The command line interface is still pretty basic and accepts the parameter only in the specific order shown in the examples.

If a passed sudoku is unsolvable the positions that it can resolve will be printed as the bottom symbol "⊥".
