
```python
import network, pickle, util

net = network.PolicyValue(network.PolicyValue.create_network())
```

```python
state = pickle.load(open("board1.pkl"))
print util.pprint_board(state.board)
```

```text
   A B C D E F G H I 
 9 . . . . . . O . . 9
 8 . . . . . . O . O 8
 7 . . . . . . O . . 7
 6 . . O . X . . O X 6
 5 . . . . . . O . . 5
 4 X X X . . X . . . 4
 3 . X . . . . . . O 3
 2 . . . X X X . O . 2
 1 O . . . . . . . . 1
   A B C D E F G H I 
None
```

```python
res = net.eval_value_state(state)
print res
```

```text
-0.000485497
```

```python
state = pickle.load(open("board1.pkl"))
print util.pprint_board(state.board)
state.current_player = 0
print state.current_player
print state._create_neighbors_cache()
print state.get_legal_moves()
#res = net.eval_policy_state(state)
#print res
```

```text
   A B C D E F G H I 
 9 . . . . . . O . . 9
 8 . . . . . . O . O 8
 7 . . . . . . O . . 7
 6 . . O . X . . O X 6
 5 . . . . . . O . . 5
 4 X X X . . X . . . 4
 3 . X . . . . . . O 3
 2 . . . X X X . O . 2
 1 O . . . . . . . . 1
   A B C D E F G H I 
None
0
None
[None, (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 7), (0, 8), (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 7), (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (2, 7), (2, 8), (3, 0), (3, 1), (3, 3), (3, 5), (3, 6), (4, 0), (4, 1), (4, 2), (4, 4), (4, 5), (4, 7), (4, 8), (5, 3), (5, 4), (5, 6), (5, 7), (5, 8), (6, 0), (6, 2), (6, 3), (6, 4), (6, 5), (6, 7), (7, 0), (7, 1), (7, 2), (7, 6), (7, 8), (8, 1), (8, 2), (8, 3), (8, 4), (8, 5), (8, 7)]
```










