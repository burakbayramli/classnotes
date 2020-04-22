from keras.layers import Input, Dense, concatenate
from keras.layers import LSTMCell, RNN
from keras.layers import Input, LSTM, RepeatVector
from keras.models import Model

latent_dim = 20; timesteps = 28; input_dim = 28; hist_dim = 5

def get_model():
    inputs = Input(shape=(timesteps, input_dim))

    encoded = LSTM(latent_dim,return_sequences=True)(inputs)
    decoded = encoded
    decoded = LSTM(input_dim, return_sequences=True)(decoded)

    seq_autoencoder = Model(inputs, decoded)

    encoder = Model(inputs, encoded)
    return seq_autoencoder, encoder

if __name__ == "__main__": 
  
    from keras.datasets import mnist
    import numpy as np
    (x_train, _), (x_test, _) = mnist.load_data()

    x_train = x_train.astype('float32') / 255.
    x_test = x_test.astype('float32') / 255.
    print (x_train.shape)
    print (x_test.shape)

    seq_autoencoder, encoder = get_model()

    seq_autoencoder.compile(optimizer='adadelta', loss='binary_crossentropy')

    seq_autoencoder.fit(x_train, x_train,
                        epochs=50,
                        batch_size=256,
                        shuffle=True,
                        validation_data=(x_test, x_test))

    seq_autoencoder.save('mod-rnn-autoenc-sim.h5')
    encoder.save('mod-rnn-enc-sim.h5')

