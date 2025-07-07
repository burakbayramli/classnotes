"""
Based on https://github.com/keras-team/keras/blob/master/examples/variational_autoencoder.py
"""
import keras
from keras.datasets import mnist
import numpy as np
from keras import backend as K
from keras.models import Sequential, Model
from keras.layers import Input, LSTM, RepeatVector
from keras.layers.core import Flatten, Dense, Dropout, Lambda
from keras.optimizers import SGD, RMSprop, Adam
from keras import objectives
import numpy as np

timesteps = 28; input_dim = 28;
batch_size = 1
latent_dim = 30

def create_lstm_vae(input_dim, 
    timesteps, 
    batch_size, 
    intermediate_dim, 
    latent_dim,
    epsilon_std=1.):

    x = Input(shape=(timesteps, input_dim,))
    print (x)
    
    # LSTM encoding
    h = LSTM(intermediate_dim)(x)
    print (h)

    # VAE Z layer
    z_mean = Dense(latent_dim)(h)
    z_log_sigma = Dense(latent_dim)(h)
    
    def sampling(args):
        z_mean, z_log_sigma = args
        epsilon = K.random_normal(shape=(batch_size, latent_dim),
                                  mean=0., stddev=epsilon_std)
        return z_mean + z_log_sigma * epsilon

    z = Lambda(sampling, output_shape=(latent_dim,))([z_mean, z_log_sigma])
    print (z)
    
    # decoded LSTM layer
    decoder_h = LSTM(intermediate_dim, return_sequences=True)
    decoder_mean = LSTM(input_dim, return_sequences=True)

    h_decoded = RepeatVector(timesteps)(z)
    print (h_decoded)
    h_decoded = decoder_h(h_decoded)
    print (h_decoded)

    # decoded layer
    x_decoded_mean = decoder_mean(h_decoded)
    print (x_decoded_mean)
    
    # end-to-end autoencoder
    vae = Model(x, x_decoded_mean)

    # encoder, from inputs to latent space
    encoder = Model(x, z_mean)

    # generator, from latent space to reconstructed inputs
    decoder_input = Input(shape=(latent_dim,))
    print (decoder_input)

    _h_decoded = RepeatVector(timesteps)(decoder_input)
    print (_h_decoded)
    _h_decoded = decoder_h(_h_decoded)
    print (_h_decoded)

    _x_decoded_mean = decoder_mean(_h_decoded)
    generator = Model(decoder_input, _x_decoded_mean)
    
    def vae_loss(x, x_decoded_mean):
        xent_loss = objectives.mse(x, x_decoded_mean)
        kl_loss = - 0.5 * K.mean(1 + z_log_sigma - K.square(z_mean) - K.exp(z_log_sigma))
        loss = xent_loss + kl_loss
        return loss

    
    vae.compile(optimizer='rmsprop', loss=vae_loss)
    
    return vae, encoder, generator


if __name__ == "__main__":
    
    (x_train, _), (x_test, _) = mnist.load_data()
    x_train = x_train.astype('float32') / 255.
    #x_train = x_train[:200]
    x_test = x_test.astype('float32') / 255.
    #x_test = x_test[:200]
    print (x_train.shape)
    print (x_test.shape)    
    x = x_train
    vae, enc, gen = create_lstm_vae(input_dim, 
        timesteps=timesteps, 
        batch_size=batch_size, 
        intermediate_dim=latent_dim,
        latent_dim=latent_dim,
        epsilon_std=1.)

    vae.fit(x, x, validation_data=(x_test, x_test), epochs=30)

    vae.save('mnist_lstm_vae.h5')
    enc.save('mnist_lstm_enc.h5')
    gen.save('mnist_lstm_gen.h5')
    

    #preds = vae.predict(x, batch_size=batch_size)


