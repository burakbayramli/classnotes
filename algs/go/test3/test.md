
```python
import pickle, util
state, pi, reward = pickle.load(open("testdata.pkl"))
state._create_neighbors_cache()
print util.pprint_board(state.board)
```

```text
   A B C D E F G H I 
 9 . X . . X O . X . 9
 8 . . . . . . . . . 8
 7 O . . . . . O . O 7
 6 . . . . . . . O . 6
 5 O . . X X . . . . 5
 4 . . X O . . O . . 4
 3 . . X . . . . . . 3
 2 . O . . X . . X . 2
 1 . O . . . . . X . 1
   A B C D E F G H I 
None
```


```python

```









