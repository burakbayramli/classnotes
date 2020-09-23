```python
def gnu_to_agz(action):
    if action==(0,0): return None
    (x,y) = action
    return (9-y,(x-1))

def agz_to_gnu(action):
    if not action: return (0,0)
    (x,y) = action
    return (y+1,9-x)


print gnu_to_agz((4,3))
print agz_to_gnu((6,3))
print agz_to_gnu((5,7))
```

```text
(6, 3)
(4, 3)
(8, 4)
```

```python

import pickle, go, util

#state, pi, reward = pickle.load(open("testdata.pkl"))
state = go.GameState()
state._create_neighbors_cache()

util.pprint_board(state.board)
#state.do_move((3,3), go.WHITE)
#state.do_move(gnu_to_agz((4,3)), go.WHITE)
```

```text
   A B C D E F G H I 
 9 . . . . . . . . . 9
 8 . . . . . . . . . 8
 7 . . . . . . . . . 7
 6 . . . . . . . . . 6
 5 . . . . . . . . . 5
 4 . . . . . . . . . 4
 3 . . . . . . . . . 3
 2 . . . . . . . . . 2
 1 . . . . . . . . . 1
   A B C D E F G H I 
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






