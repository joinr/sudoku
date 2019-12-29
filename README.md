# sudoku

A slightly different take on Dr. Norvig's 
[Sudoku puzzle solver](https://norvig.com/sudoku.html)


## Algorithm Outline

Starting from Norvig's algorithm, I essentially do the same thing.  I mutably update
a board on each choice keeping track of the possible moves to other locations and
which parts of the board have a chosen number.


I represent the board as an y,x 2d indexed object array of one of three things:
* `nil` - broken constraint
* `number` - chosen value
* `set` - potential values


I used an [optimized set implementation](src/sudoku/longset.clj) where each set
consists of a single long value and I check for which bits are high in order to
decide inclusion to the set.  This works assuming you only want numbers 0-63 in your
set which is plenty for sudoku.



I also create an index tensor to pre-calculate the sets of affected indexes.


I precalculate all of the potential indexes that can be effected by any given move.


When choosing values, I iterate over the affected list of indexes which must include
the desired index to chose and mutably update an object array keeping track of if
the result is valid and which entries need a second pass for constraint propagation.

Constraint propagation is then a reduction over the constraint propagation list, updating
the board mutably or returning nil if a constraint is broken.


The intention was play with this problem from a more numeric perspective.  My hope
is this code isn't too terse or hard to read.

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
