
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
Tensor("input_3:0", shape=(?, 17, 9, 9), dtype=float32)
Tensor("conv2d_9/BiasAdd:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("batch_normalization_9/batchnorm/add_1:0", shape=(?, 17, 9, 16), dtype=float32)
Tensor("conv2d_10/BiasAdd:0", shape=(?, 17, 9, 32), dtype=float32)
Tensor("batch_normalization_10/batchnorm/add_1:0", shape=(?, 17, 9, 32), dtype=float32)
------------- value -------------------
Tensor("conv2d_11/BiasAdd:0", shape=(?, 17, 9, 2), dtype=float32)
Tensor("activation_17/Relu:0", shape=(?, 17, 9, 2), dtype=float32)
Tensor("flatten_5/Reshape:0", shape=(?, 306), dtype=float32)
policy_output Tensor("activation_18/Softmax:0", shape=(?, 82), dtype=float32)
------------- policy -------------------
Tensor("conv2d_12/BiasAdd:0", shape=(?, 17, 9, 1), dtype=float32)
Tensor("activation_19/Relu:0", shape=(?, 17, 9, 1), dtype=float32)
Tensor("flatten_6/Reshape:0", shape=(?, 153), dtype=float32)
Tensor("dense_8/BiasAdd:0", shape=(?, 256), dtype=float32)
Tensor("activation_20/Relu:0", shape=(?, 256), dtype=float32)
Tensor("dense_9/BiasAdd:0", shape=(?, 1), dtype=float32)
Tensor("dense_9/BiasAdd:0", shape=(?, 1), dtype=float32)
```

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
from tensorflow.contrib.keras import optimizers as O
from tensorflow.contrib.keras import callbacks as C
from tensorflow.contrib.keras import backend as K

def lr_scheduler(epoch):
    if epoch == 400000:
        K.set_value(model.optimizer.lr, .001)
    elif epoch == 600000:
        K.set_value(model.optimizer.lr, .0001)
    return K.get_value(model.optimizer.lr)
	
change_lr = C.LearningRateScheduler(lr_scheduler)
sgd = O.SGD(lr=.01, momentum=0.9)
net.model.compile(loss=['categorical_crossentropy','mean_squared_error'], optimizer=sgd)
```


```python
batch_size = 1
pout = np.zeros((batch_size, 9*9+1))
pout[0,:] = util.to_pi_mat(pi)
vout = np.zeros((batch_size, 1))
vout[0,:] = reward
Y = [pout, vout]
X = np.zeros((batch_size, 17, 9, 9))
X[0, :] = util.get_board(state)
net.model.fit(X, Y)
```

```text
Epoch 1/1
1/1 [==============================] - 1s - loss: 8.2936 - activation_18_loss: 4.6405 - activation_21_loss: 3.6121
Out[1]: <tensorflow.python.keras._impl.keras.callbacks.History at 0x7fe8e62908d0>
```







