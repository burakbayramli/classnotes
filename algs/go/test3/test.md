


```python
import resnet, pickle, util
net = resnet.PolicyValue(resnet.PolicyValue.create_network())
```

```text
Tensor("input_3:0", shape=(?, 17, 9, 9), dtype=float32)
Tensor("conv2d_43/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_43/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_44/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_44/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_50/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_45/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_45/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_19/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_51/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_46/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_46/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_52/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_47/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_47/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_20/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_53/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_48/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_48/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_54/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_49/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_49/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_21/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_55/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_50/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_50/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_56/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_51/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_51/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_22/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_57/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_52/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_52/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_58/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_53/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_53/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_23/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_59/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_54/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_54/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_60/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_55/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_55/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_24/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_61/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_56/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_56/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_62/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_57/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_57/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_25/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_63/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_58/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_58/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_64/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_59/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_59/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_26/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_65/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_60/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_60/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_66/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_61/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_61/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("add_27/add:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("activation_67/Relu:0", shape=(?, 17, 9, 16), dtype=float32)
------------- policy -------------------
Tensor("conv2d_62/BiasAdd:0", shape=(?, 17, 9, 2), dtype=float32)
Tensor("activation_68/Relu:0", shape=(?, 17, 9, 2), dtype=float32)
Tensor("flatten_5/Reshape:0", shape=(?, 306), dtype=float32)
policy_output Tensor("activation_69/Softmax:0", shape=(?, 82), dtype=float32)
------------- policy -------------------
Tensor("conv2d_63/BiasAdd:0", shape=(?, 17, 9, 1), dtype=float32)
Tensor("activation_70/Relu:0", shape=(?, 17, 9, 1), dtype=float32)
Tensor("flatten_6/Reshape:0", shape=(?, 153), dtype=float32)
Tensor("dense_8/BiasAdd:0", shape=(?, 256), dtype=float32)
Tensor("activation_71/Relu:0", shape=(?, 256), dtype=float32)
Tensor("dense_9/BiasAdd:0", shape=(?, 1), dtype=float32)
Tensor("dense_9/BiasAdd:0", shape=(?, 1), dtype=float32)
```

```python
state = pickle.load(open("board1.pkl"))
state._create_neighbors_cache()
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
0.803456
```

```python
import network, pickle, util, go

state = pickle.load(open("board1.pkl"))
#state.current_player = go.BLACK
print state.current_player
lm = state.get_legal_moves()
print len(lm), lm
res = net.eval_policy_state(state)
print res
```

```text
1
62 [None, (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 7), (0, 8), (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 7), (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (2, 7), (2, 8), (3, 0), (3, 1), (3, 3), (3, 5), (3, 6), (4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (4, 7), (4, 8), (5, 3), (5, 4), (5, 6), (5, 7), (5, 8), (6, 0), (6, 2), (6, 3), (6, 4), (6, 5), (6, 6), (6, 7), (7, 0), (7, 1), (7, 2), (7, 6), (7, 8), (8, 1), (8, 2), (8, 3), (8, 4), (8, 5), (8, 6), (8, 7), (8, 8)]
[0.00059019827, 0.0061528366, 0.023501551, 0.0051653204, 0.0041054208, 0.0014641138, 0.0019243082, 0.036704369, 0.00057243626, 0.031468783, 0.00075256237, 0.0016225742, 0.068846971, 0.0088369921, 0.0012060822, 0.012769492, 0.014729434, 0.012070517, 0.00035979468, 0.0079417937, 0.012058574, 0.0079989629, 0.002515546, 0.0044319183, 0.0018773929, 0.0097521804, 0.013427786, 0.047561217, 0.00046689392, 0.1153298, 0.00161595, 0.013073359, 0.0043753898, 0.11275286, 0.00070595962, 0.0031019931, 0.00082974677, 0.0015053105, 0.010339521, 0.0012516024, 0.028607352, 0.026982004, 0.0020673866, 0.0005492561, 0.009953157, 0.0016175937, 0.00051040156, 0.0047976626, 0.015361131, 0.0034852971, 0.0068298834, 0.0026259094, 0.00044995185, 0.00072519924, 0.010034629, 0.0040321173, 0.0015689885, 0.0047178767, 0.00049343513, 0.01525555, 0.01000932]
```










