
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
 9 . . . X . . O . . 9
 8 . . . . . . . . . 8
 7 X . X . . O . . . 7
 6 O . X . . . . . . 6
 5 O . . X . . . . . 5
 4 . . X X . . X . . 4
 3 O O O . O . . . . 3
 2 X . . . . O . O . 2
 1 . . X . . . . . . 1
   A B C D E F G H I 
None
```

```python
res = net.eval_value_state(state)
print res
```

```text
-0.00221886
```

```python
print res[0].shape
print res[1].shape
```

```text
(1, 82)
(1, 1)
```

```python
res = net.eval_policy_state(state)
print res
```

```text
[(None, -0.28896204)]
```


