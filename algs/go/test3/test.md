
##########################################
x = Dense(64, activation='relu')(input_vecs)
x = Dropout(0.5)(x)
x = Dense(32, activation='relu')(x)
y = Dense(1)(x)

model = Model(input=[user_id_input, item_id_input, meta_input], output=y)
model.compile(optimizer='adam', loss='mae')

initial_train_preds = model.predict([user_id_train, item_id_train, item_meta_train])

history = model.fit([user_id_train, item_id_train, item_meta_train], rating_train,
                    batch_size=64, nb_epoch=15, validation_split=0.1,
                    shuffle=True)

################################################3

from tensorflow.contrib.keras import callbacks as C
from tensorflow.contrib.keras import backend as K
change_lr = C.LearningRateScheduler(lr_scheduler)
sgd = O.SGD(lr=.01, momentum=0.9)
model.compile(loss=['categorical_crossentropy','mean_squared_error'], optimizer=sgd)

################################################3





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










