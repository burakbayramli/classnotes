
```python
import pickle, util
state, pi, reward = pickle.load(open("testdata.pkl"))
state._create_neighbors_cache()
print util.pprint_board(state.board)
net = simplenet.PolicyValue(simplenet.PolicyValue.create_network())
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
Tensor("input_2:0", shape=(?, 17, 9, 9), dtype=float32)
Tensor("conv2d_5/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_5/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_6/BiasAdd:0", shape=(?, 17, 9, 32), dtype=float32)
Tensor("batch_normalization_6/batchnorm/add_1:0", shape=(?, 17, 9, 32), dtype=float32)
------------- value -------------------
Tensor("conv2d_7/BiasAdd:0", shape=(?, 17, 9, 2), dtype=float32)
Tensor("activation_10/Relu:0", shape=(?, 17, 9, 2), dtype=float32)
Tensor("flatten_3/Reshape:0", shape=(?, 306), dtype=float32)
policy_output Tensor("activation_11/Softmax:0", shape=(?, 82), dtype=float32)
------------- policy -------------------
Tensor("conv2d_8/BiasAdd:0", shape=(?, 17, 9, 1), dtype=float32)
Tensor("activation_12/Relu:0", shape=(?, 17, 9, 1), dtype=float32)
Tensor("flatten_4/Reshape:0", shape=(?, 153), dtype=float32)
Tensor("dense_5/BiasAdd:0", shape=(?, 256), dtype=float32)
Tensor("activation_13/Relu:0", shape=(?, 256), dtype=float32)
Tensor("dense_6/BiasAdd:0", shape=(?, 1), dtype=float32)
Tensor("dense_6/BiasAdd:0", shape=(?, 1), dtype=float32)
```

```python

```










