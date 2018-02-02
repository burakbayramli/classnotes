
```python
import network, pickle, util

x = network.PolicyValue(network.PolicyValue.create_network())
```

```python
state = pickle.load(open("board1.pkl"))
print util.pprint_board(state.board)
```

```text
   A B C D E F G H I 
 9 . . O . . . . . X 9
 8 X O X . . X . . . 8
 7 X . . . . . . . . 7
 6 O . . . . . . . . 6
 5 . X . O . . . . . 5
 4 . . O . . . . X . 4
 3 . . . . . . . . . 3
 2 O O . . . X . . X 2
 1 . . O . O . . . O 1
   A B C D E F G H I 
None
```








