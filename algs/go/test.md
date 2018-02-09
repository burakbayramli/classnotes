
```python
import gtp
gnugo = gtp.GTPFacade("white", ["gnugo", "--mode", "gtp", "--level", "10"])

gnugo.boardsize(9)
gnugo.komi(5.5)
gnugo.clear_board()
```

```text
white subprocess created
sending white: boardsize 9

got: = 

sending white: komi 5.5

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

got: = G7

res (7, 7)
sending white: showboard

got: = 
   A B C D E F G H J
 9 . . . . . . . . . 9
 8 . . . . . . . . . 8
 7 . . O O . . O . . 7
 6 . . . . . . . . . 6
 5 . . . . + . . . . 5
 4 . . . O . . . . . 4
 3 . . + O . . + . . 3
 2 . . . . . . . . . 2     WHITE (O) has captured 0 stones
 1 . . . . . . . . . 1     BLACK (X) has captured 0 stones
   A B C D E F G H J

```


```python
gnugo.play(gtp.BLACK, (2,2))
gnugo.showboard()
```

```text
sending white: play B B2

got: = 

sending white: showboard

got: = 
   A B C D E F G H J
 9 . . . . . . . . . 9
 8 . . . . . . . . . 8
 7 . . O O . . O . . 7
 6 . . . . . . . . . 6
 5 . . . . + . . . . 5
 4 . . . O . . . . . 4
 3 . . + O . . + . . 3
 2 . X . . . . . . . 2     WHITE (O) has captured 0 stones
 1 . . . . . . . . . 1     BLACK (X) has captured 0 stones
   A B C D E F G H J

```






