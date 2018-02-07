
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
print pi
```

```text
[((7, 3), 0), ((4, 7), 0), ((1, 3), 0), ((4, 8), 0), ((3, 0), 0), ((8, 0), 0), ((5, 4), 0), ((2, 1), 0), ((1, 6), 0), ((5, 1), 0), ((2, 5), 0), ((8, 5), 0), ((0, 3), 0), ((5, 8), 0), ((8, 2), 0), ((1, 2), 0), ((7, 8), 0), ((6, 7), 0), ((3, 3), 0), ((7, 6), 0), ((6, 6), 0), ((6, 3), 0), ((1, 5), 0), ((7, 2), 0), ((3, 6), 0), ((2, 2), 0), ((5, 7), 0), ((4, 1), 0), ((1, 1), 0), ((6, 4), 0), ((3, 2), 0), ((0, 0), 0), ((5, 0), 0), ((4, 5), 0), ((8, 6), 0), (None, 0), ((1, 4), 0), ((6, 0), 0), ((7, 5), 1), ((2, 3), 0), ((6, 8), 0), ((4, 2), 0), ((1, 0), 0), ((0, 8), 0), ((6, 5), 0), ((3, 5), 0), ((2, 7), 0), ((8, 3), 0), ((7, 0), 0), ((4, 6), 0), ((5, 5), 0), ((6, 1), 0), ((3, 1), 0), ((0, 2), 0), ((3, 8), 0), ((0, 6), 0), ((1, 8), 0), ((8, 8), 0), ((1, 7), 0), ((3, 4), 0), ((2, 4), 0), ((8, 4), 0)]
```

```python
res = util.to_pi_mat(pi)
print res
```

```text
[ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.
  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.
  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.
  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  1.  0.  0.
  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
```



```python
import simplenet, pickle, util
net = simplenet.PolicyValue(simplenet.PolicyValue.create_network())
```

```text
Tensor("input_1:0", shape=(?, 17, 9, 9), dtype=float32)
Tensor("conv2d/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_2/BiasAdd:0", shape=(?, 17, 9, 32), dtype=float32)
Tensor("batch_normalization_2/batchnorm/add_1:0", shape=(?, 17, 9, 32), dtype=float32)
------------- value -------------------
Tensor("conv2d_3/BiasAdd:0", shape=(?, 17, 9, 2), dtype=float32)
Tensor("activation_3/Relu:0", shape=(?, 17, 9, 2), dtype=float32)
Tensor("flatten/Reshape:0", shape=(?, 306), dtype=float32)
policy_output Tensor("activation_4/Softmax:0", shape=(?, 82), dtype=float32)
------------- policy -------------------
Tensor("conv2d_4/BiasAdd:0", shape=(?, 17, 9, 1), dtype=float32)
Tensor("activation_5/Relu:0", shape=(?, 17, 9, 1), dtype=float32)
Tensor("flatten_2/Reshape:0", shape=(?, 153), dtype=float32)
Tensor("dense_2/BiasAdd:0", shape=(?, 256), dtype=float32)
Tensor("activation_6/Relu:0", shape=(?, 256), dtype=float32)
Tensor("dense_3/BiasAdd:0", shape=(?, 1), dtype=float32)
Tensor("dense_3/BiasAdd:0", shape=(?, 1), dtype=float32)
```

```python
import resnet, pickle, util
net = resnet.PolicyValue(resnet.PolicyValue.create_network())
```

```text
Tensor("input_2:0", shape=(?, 17, 9, 9), dtype=float32)
Tensor("conv2d_5/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_5/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_6/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_6/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_9/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_7/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_7/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_10/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_8/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_8/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_11/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_9/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_9/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_2/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_12/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_10/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_10/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_13/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_11/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_11/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_3/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_14/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_12/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_12/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_15/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_13/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_13/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_4/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_16/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_14/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_14/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_17/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_15/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_15/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_5/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_18/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_16/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_16/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_19/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_17/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_17/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_6/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_20/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_18/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_18/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_21/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_19/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_19/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_7/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_22/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_20/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_20/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_23/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_21/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_21/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_8/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_24/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_22/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_22/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_25/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_23/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_23/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_9/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_26/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
------------- policy -------------------
Tensor("conv2d_24/BiasAdd:0", shape=(?, 17, 9, 2), dtype=float32)
Tensor("activation_27/Relu:0", shape=(?, 17, 9, 2), dtype=float32)
Tensor("flatten_3/Reshape:0", shape=(?, 306), dtype=float32)
policy_output Tensor("activation_28/Softmax:0", shape=(?, 82), dtype=float32)
------------- policy -------------------
Tensor("conv2d_25/BiasAdd:0", shape=(?, 17, 9, 1), dtype=float32)
Tensor("activation_29/Relu:0", shape=(?, 17, 9, 1), dtype=float32)
Tensor("flatten_4/Reshape:0", shape=(?, 153), dtype=float32)
Tensor("dense_5/BiasAdd:0", shape=(?, 256), dtype=float32)
Tensor("activation_30/Relu:0", shape=(?, 256), dtype=float32)
Tensor("dense_6/BiasAdd:0", shape=(?, 1), dtype=float32)
Tensor("dense_6/BiasAdd:0", shape=(?, 1), dtype=float32)
```


```python
res = net.eval_value_state(state)
print res
```

```text
0.758366
```

```python
import network, pickle, util, go

print state.current_player
lm = state.get_legal_moves()
print len(lm), lm
res = net.eval_policy_state(state)
print res
```

```text
1
62 [None, (0, 0), (0, 2), (0, 3), (0, 6), (0, 8), (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7), (1, 8), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (2, 7), (3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5), (3, 6), (3, 8), (4, 1), (4, 2), (4, 5), (4, 6), (4, 7), (4, 8), (5, 0), (5, 1), (5, 4), (5, 5), (5, 7), (5, 8), (6, 0), (6, 1), (6, 3), (6, 4), (6, 5), (6, 6), (6, 7), (6, 8), (7, 0), (7, 2), (7, 3), (7, 5), (7, 6), (7, 8), (8, 0), (8, 2), (8, 3), (8, 4), (8, 5), (8, 6), (8, 8)]
[((0, 0), 0.012195121), ((0, 2), 0.012195121), ((0, 3), 0.012195121), ((0, 6), 0.012195121), ((0, 8), 0.012195121), ((1, 0), 0.012195121), ((1, 1), 0.012195121), ((1, 2), 0.012195121), ((1, 3), 0.012195121), ((1, 4), 0.012195121), ((1, 5), 0.012195121), ((1, 6), 0.012195121), ((1, 7), 0.012195121), ((1, 8), 0.012195121), ((2, 1), 0.012195121), ((2, 2), 0.012195121), ((2, 3), 0.012195121), ((2, 4), 0.012195121), ((2, 5), 0.012195121), ((2, 7), 0.012195121), ((3, 0), 0.012195121), ((3, 1), 0.012195121), ((3, 2), 0.012195121), ((3, 3), 0.012195121), ((3, 4), 0.012195121), ((3, 5), 0.012195121), ((3, 6), 0.012195121), ((3, 8), 0.012195121), ((4, 1), 0.012195121), ((4, 2), 0.012195121), ((4, 5), 0.012195121), ((4, 6), 0.012195121), ((4, 7), 0.012195121), ((4, 8), 0.012195121), ((5, 0), 0.012195121), ((5, 1), 0.012195121), ((5, 4), 0.012195121), ((5, 5), 0.012195121), ((5, 7), 0.012195121), ((5, 8), 0.012195121), ((6, 0), 0.012195121), ((6, 1), 0.012195121), ((6, 3), 0.012195121), ((6, 4), 0.012195121), ((6, 5), 0.012195121), ((6, 6), 0.012195121), ((6, 7), 0.012195121), ((6, 8), 0.012195121), ((7, 0), 0.012195121), ((7, 2), 0.012195121), ((7, 3), 0.012195121), ((7, 5), 0.012195121), ((7, 6), 0.012195121), ((7, 8), 0.012195121), ((8, 0), 0.012195121), ((8, 2), 0.012195121), ((8, 3), 0.012195121), ((8, 4), 0.012195121), ((8, 5), 0.012195121), ((8, 6), 0.012195121), ((8, 8), 0.012195121), (None, 0.012195121)]
```










