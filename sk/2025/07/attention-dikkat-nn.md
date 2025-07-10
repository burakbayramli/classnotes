# Dikkat (Attention) Modelleri, YSA

```python
from pandas import read_csv
import numpy as np, keras, tensorflow as tf
from keras import Model
from keras.layers import Layer
import keras.backend as K
from keras.layers import Input, Dense, SimpleRNN
from sklearn.preprocessing import MinMaxScaler
from keras.models import Sequential
from keras.metrics import mean_squared_error

print ('Keras',keras.__version__)
print ('TF',tf.__version__)

# Prepare data
def get_fib_seq(n, scale_data=True):
    # Get the Fibonacci sequence
    seq = np.zeros(n)
    fib_n1 = 0.0
    fib_n = 1.0 
    for i in range(n):
            seq[i] = fib_n1 + fib_n
            fib_n1 = fib_n
            fib_n = seq[i] 
    scaler = []
    if scale_data:
        scaler = MinMaxScaler(feature_range=(0, 1))
        seq = np.reshape(seq, (n, 1))
        seq = scaler.fit_transform(seq).flatten()        
    return seq, scaler

def get_fib_XY(total_fib_numbers, time_steps, train_percent, scale_data=True):
    dat, scaler = get_fib_seq(total_fib_numbers, scale_data)    
    Y_ind = np.arange(time_steps, len(dat), 1)
    Y = dat[Y_ind]
    rows_x = len(Y)
    X = dat[0:rows_x]
    for i in range(time_steps-1):
        temp = dat[i+1:rows_x+i+1]
        X = np.column_stack((X, temp))
    # random permutation with fixed seed   
    rand = np.random.RandomState(seed=13)
    idx = rand.permutation(rows_x)
    split = int(train_percent*rows_x)
    train_ind = idx[0:split]
    test_ind = idx[split:]
    trainX = X[train_ind]
    trainY = Y[train_ind]
    testX = X[test_ind]
    testY = Y[test_ind]
    trainX = np.reshape(trainX, (len(trainX), time_steps, 1))    
    testX = np.reshape(testX, (len(testX), time_steps, 1))
    return trainX, trainY, testX, testY, scaler

# Set up parameters
time_steps = 20
hidden_units = 2
epochs = 30

# Create a traditional RNN network
def create_RNN(hidden_units, dense_units, input_shape, activation):
    model = Sequential()
    model.add(SimpleRNN(hidden_units, input_shape=input_shape, activation=activation[0]))
    model.add(Dense(units=dense_units, activation=activation[1]))
    model.compile(loss='mse', optimizer='adam')
    return model

model_RNN = create_RNN(hidden_units=hidden_units, dense_units=1, input_shape=(time_steps,1), 
                   activation=['tanh', 'tanh'])

# Generate the dataset for the network
trainX, trainY, testX, testY, scaler  = get_fib_XY(1200, time_steps, 0.7)
# Train the network
model_RNN.fit(trainX, trainY, epochs=epochs, batch_size=1, verbose=2)


# Evalute model
train_mse = model_RNN.evaluate(trainX, trainY)
test_mse = model_RNN.evaluate(testX, testY)

# Print error
print("Train set MSE Without Attn = ", train_mse)
print("Test set MSE Without Attn = ", test_mse)


# Add attention layer to the deep learning network
class attention(Layer):
    def __init__(self,**kwargs):
        super(attention,self).__init__(**kwargs)

    def build(self,input_shape):
        self.W=self.add_weight(name='attention_weight', shape=(input_shape[-1],1), 
                               initializer='random_normal', trainable=True)
        self.b=self.add_weight(name='attention_bias', shape=(input_shape[1],1), 
                               initializer='zeros', trainable=True)        
        super(attention, self).build(input_shape)

    def call(self,x):
        # Alignment scores. Pass them through tanh function
        e = K.tanh(K.dot(x,self.W)+self.b)
        # Remove dimension of size 1
        e = K.squeeze(e, axis=-1)   
        # Compute the weights
        alpha = K.softmax(e)
        # Reshape to tensorFlow format
        alpha = K.expand_dims(alpha, axis=-1)
        # Compute the context vector
        context = x * alpha
        context = K.sum(context, axis=1)
        return context
    
def create_RNN_with_attention(hidden_units, dense_units, input_shape, activation):
    x=Input(shape=input_shape)
    RNN_layer = SimpleRNN(hidden_units, return_sequences=True, activation=activation)(x)
    attention_layer = attention()(RNN_layer)
    outputs=Dense(dense_units, trainable=True, activation=activation)(attention_layer)
    model=Model(x,outputs)
    model.compile(loss='mse', optimizer='adam')    
    return model    

# Create the model with attention, train and evaluate
model_attention = create_RNN_with_attention(hidden_units=hidden_units, dense_units=1, 
                                  input_shape=(time_steps,1), activation='tanh')
model_attention.summary()    


model_attention.fit(trainX, trainY, epochs=epochs, batch_size=1, verbose=2)

# Evalute model
train_mse_attn = model_attention.evaluate(trainX, trainY)
test_mse_attn = model_attention.evaluate(testX, testY)

# Print error
print("Train set MSE with attention = ", train_mse_attn)
print("Test set MSE with attention = ", test_mse_attn)
```

```
Keras 2.15.0
TF 2.15.1
Epoch 1/30
826/826 - 4s - loss: 0.0035 - 4s/epoch - 4ms/step
Epoch 2/30
826/826 - 3s - loss: 0.0034 - 3s/epoch - 3ms/step
Epoch 3/30
...
Epoch 26/30
826/826 - 3s - loss: 0.0010 - 3s/epoch - 3ms/step
Epoch 27/30
826/826 - 3s - loss: 9.4093e-04 - 3s/epoch - 3ms/step
Epoch 28/30
826/826 - 3s - loss: 8.6900e-04 - 3s/epoch - 3ms/step
Epoch 29/30
826/826 - 3s - loss: 7.9342e-04 - 3s/epoch - 3ms/step
Epoch 30/30
826/826 - 3s - loss: 7.1211e-04 - 3s/epoch - 3ms/step
1/26 [>.............................] - ETA: 6s - loss: 2.7926e-04
22/26 [========================>.....] - ETA: 0s - loss: 7.3791e-04
26/26 [==============================] - 0s 2ms/step - loss: 6.2892e-04
1/12 [=>............................] - ETA: 0s - loss: 7.3267e-04
12/12 [==============================] - 0s 2ms/step - loss: 5.3967e-04
Train set MSE Without Attn =  0.0006289228913374245
Test set MSE Without Attn =  0.0005396747146733105
Model: "model"
_________________________________________________________________
 Layer (type)                Output Shape              Param #   
=================================================================
 input_1 (InputLayer)        [(None, 20, 1)]           0         
                                                                 
 simple_rnn_1 (SimpleRNN)    (None, 20, 2)             8         
                                                                 
 attention (attention)       (None, 2)                 22        
                                                                 
 dense_1 (Dense)             (None, 1)                 3         
                                                                 
=================================================================
Total params: 33 (132.00 Byte)
Trainable params: 33 (132.00 Byte)
Non-trainable params: 0 (0.00 Byte)
_________________________________________________________________
Epoch 1/30
826/826 - 4s - loss: 0.0011 - 4s/epoch - 5ms/step
Epoch 2/30
826/826 - 3s - loss: 0.0011 - 3s/epoch - 4ms/step
Epoch 3/30
826/826 - 3s - loss: 0.0011 - 3s/epoch - 4ms/step
Epoch 4/30
826/826 - 3s - loss: 0.0010 - 3s/epoch - 3ms/step
Epoch 5/30
826/826 - 3s - loss: 9.8559e-04 - 3s/epoch - 3ms/step
Epoch 6/30
826/826 - 3s - loss: 9.4220e-04 - 3s/epoch - 4ms/step
Epoch 7/30
826/826 - 3s - loss: 8.8950e-04 - 3s/epoch - 4ms/step
Epoch 8/30
826/826 - 3s - loss: 8.5065e-04 - 3s/epoch - 4ms/step
Epoch 9/30
..
Epoch 30/30
826/826 - 3s - loss: 6.8983e-05 - 3s/epoch - 3ms/step
 1/26 [>.............................] - ETA: 5s - loss: 4.8165e-0721/26 [=======================>......] - ETA: 0s - loss: 6.9184e-0526/26 [==============================] - 0s 3ms/step - loss: 5.6285e-05
 1/12 [=>............................] - ETA: 0s - loss: 6.3000e-0612/12 [==============================] - 0s 3ms/step - loss: 5.9010e-07
Train set MSE with attention =  5.628508370136842e-05
Test set MSE with attention =  5.901009672015789e-07
```


0.00002662211591
0.0008260479662567377

Kaynaklar

[1] https://machinelearningmastery.com/adding-a-custom-attention-layer-to-recurrent-neural-network-in-keras/

