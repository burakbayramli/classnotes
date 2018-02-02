
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

```python
res = net.eval_value_state(state)
```

```text
(1, 17, 9, 9)
```

```python
print res[0].shape
print res[1].shape
```

```text
(1, 82)
(1, 1)
```




