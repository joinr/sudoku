# sudoku

A slightly different take on Dr. Norvig's Sudoku puzzle solver -

I just use an object array as the board and generate the sets of potentially
affected indexes for any given move.  This gets me decent performance but I
was unable to beat the python implementation by enough to really care.

## Usage

```console
lein uberjar && java -jar target/sudoku.jar

warming up
solved 50 puzzles
solving easy group
solved 50 puzzles
"Elapsed time: 64.008797 msecs"
solving top95 group
solved 95 puzzles
"Elapsed time: 4475.410177 msecs"
solving hardest group
solved 11 puzzles
"Elapsed time: 23.55941 msecs"
Solving really hard one...please wait
"Elapsed time: 20667.936802 msecs"
-------------------------
| 4 3 8 | 7 9 6 | 2 1 5 |
| 6 5 9 | 1 3 2 | 4 7 8 |
| 2 7 1 | 4 5 8 | 6 9 3 |
-------------------------
| 8 4 5 | 2 1 9 | 3 6 7 |
| 7 1 3 | 5 6 4 | 8 2 9 |
| 9 2 6 | 8 7 3 | 1 5 4 |
-------------------------
| 1 9 4 | 3 2 5 | 7 8 6 |
| 3 6 2 | 9 8 7 | 5 4 1 |
| 5 8 7 | 6 4 1 | 9 3 2 |
-------------------------
```

## License

Copyright © 2019 Chris Nuernberger

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.
