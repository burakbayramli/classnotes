

```python
import network, pickle, util
net = network.PolicyValue(network.PolicyValue.create_network())
```

```text
policy_output Tensor("activation_205/Softmax:0", shape=(?, 82), dtype=float32)
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
import network, pickle, util, go

state = pickle.load(open("board1.pkl"))
#print util.pprint_board(state.board)
state.current_player = go.BLACK
#print state.current_player
#print state._create_neighbors_cache()
#lm = state.get_legal_moves()
#print len(lm), lm
print
res = net.eval_policy_state(state)
print res
```

```text

[0.015992034, 0.0073194765, 0.012287774, 0.010310446, 0.0081716087, 0.012542414, 0.0046109455, 0.0023174037, 0.011103716, 0.015903475, 0.017949382, 0.0097899977, 0.010946715, 0.012508473, 0.013879023, 0.014734574, 0.0074582528, 0.013297992, 0.014666552, 0.0089405049, 0.020288428, 0.010913274, 0.015245888, 0.013848384, 0.0030903381, 0.031382263, 0.010606343, 0.030959167, 0.01022518, 0.012176595, 0.016117891, 0.011340585, 0.011048933, 0.011910415, 0.015644729, 0.010486938, 0.014539456, 0.0078531522, 0.011757938, 0.015442742, 0.014990166, 0.0083467262, 0.0094278939, 0.01853496, 0.007271539, 0.0076878569, 0.0045317998, 0.009167158, 0.0030055959, 0.012792093, 0.0088233035, 0.0049675112, 0.022721943, 0.0084803673, 0.013157866, 0.018819151, 0.017837891, 0.011366406, 0.0062669525, 0.0072449292, 0.02297355]
```










