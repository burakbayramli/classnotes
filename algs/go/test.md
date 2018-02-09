
```python
import pickle, go, util

state, pi, reward = pickle.load(open("testdata.pkl"))
state._create_neighbors_cache()

print state.get_legal_moves()

print util.pprint_board(state.board)
state.do_move((5,3), go.WHITE)
print util.pprint_board(state.board)

#state.do_move((2,5), go.WHITE)
#print util.pprint_board(state.board)

```

```text
[None, (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (1, 0), (1, 1), (1, 2), (1, 4), (1, 5), (1, 7), (1, 8), (2, 0), (2, 1), (2, 5), (2, 6), (2, 7), (2, 8), (3, 0), (3, 4), (3, 7), (3, 8), (4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (4, 7), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5), (5, 6), (5, 7), (6, 0), (6, 1), (6, 2), (6, 3), (6, 4), (6, 5), (6, 6), (6, 7), (7, 0), (7, 1), (7, 2), (7, 4), (7, 5), (7, 8), (8, 0), (8, 1), (8, 3), (8, 4), (8, 5), (8, 6), (8, 7)]
   A B C D E F G H I 
 9 . . . . . . . . . 9
 8 . . . O . . O . . 8
 7 . . X O X . . . . 7
 6 . O X O . O X . . 6
 5 . . . . . . X . O 5
 4 O . . . . . . . O 4
 3 . . . . . . . . X 3
 2 . . . X . . X X . 2
 1 . . O . . . . . X 1
   A B C D E F G H I 
None
   A B C D E F G H I 
 9 . . . . . . . . . 9
 8 . . . O . . O . . 8
 7 . . X O X . . . . 7
 6 . O X O . O X . . 6
 5 . . . . . . X . O 5
 4 O . . O . . . . O 4
 3 . . . . . . . . X 3
 2 . . . X . . X X . 2
 1 . . O . . . . . X 1
   A B C D E F G H I 
None
```



















```python
import gtp
gnugo = gtp.GTPFacade("white", ["gnugo", "--mode", "gtp", "--level", "10"])

gnugo.boardsize(9)
gnugo.komi(0.0)
gnugo.clear_board()
```

```text
white subprocess created
sending white: boardsize 9

got: = 

sending white: komi 0.0

got: = 

sending white: clear_board

got: = 

```

```python
res = gnugo.genmove(gtp.WHITE)
print 'res',res
gnugo.showboard()
```

```text
sending white: genmove W

got: = E5

res (5, 5)
sending white: showboard

got: = 
   A B C D E F G H J
 9 . . . . . . . . . 9
 8 . . . . . . . . . 8
 7 . . + . . . + . . 7
 6 . . . . . . . . . 6
 5 . . . . O . . . . 5
 4 . . . . . . . . . 4
 3 . . + . . . + . . 3
 2 . . . . . . . . . 2     WHITE (O) has captured 0 stones
 1 . . . . . . . . . 1     BLACK (X) has captured 0 stones
   A B C D E F G H J

```


```python
gnugo.play(gtp.BLACK, (4,2))
gnugo.showboard()
```

```text
sending white: play B D2

got: = 

sending white: showboard

got: = 
   A B C D E F G H J
 9 . . . . . . . . . 9
 8 . . . . . . . . . 8
 7 . . + . . . + . . 7
 6 . . . . . . . . . 6
 5 . . . . O . . . . 5
 4 . . . . . . . . . 4
 3 . . + . . . + . . 3
 2 . . . X . . . . . 2     WHITE (O) has captured 0 stones
 1 . . . . . . . . . 1     BLACK (X) has captured 0 stones
   A B C D E F G H J

```






