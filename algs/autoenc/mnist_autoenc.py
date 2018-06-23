from keras.layers import Input, Dense
from keras.models import Model

# gizli katman
encoding_dim = 32

def get_model():
    # girdi
    input_img = Input(shape=(784,))
    # kodlanmis temsil
    encoded = Dense(encoding_dim, activation='relu')(input_img)
    # kodcozulmus temsil
    decoded = Dense(784, activation='sigmoid')(encoded)
    
    # bu model girdiyi tekrar olusturulmus hale cevirir
    autoencoder = Model(input_img, decoded)

    # bu model girdiyi kodlanmis hale getirir
    encoder = Model(input_img, encoded)

    encoded_input = Input(shape=(encoding_dim,))
    # ozkodlayicinin son tabakasini al bu kodcozulmus katman
    decoder_layer = autoencoder.layers[-1]
    # kodcozucu model
    decoder = Model(encoded_input, decoder_layer(encoded_input))

    autoencoder.compile(optimizer='adadelta', loss='binary_crossentropy')
    return autoencoder, encoder, decoder

if __name__ == "__main__": 
 
    autoencoder, encoder, decoder = get_model()

    from keras.datasets import mnist
    import numpy as np
    (x_train, _), (x_test, _) = mnist.load_data()

    x_train = x_train.astype('float32') / 255.
    x_test = x_test.astype('float32') / 255.
    x_train = x_train.reshape((len(x_train), np.prod(x_train.shape[1:])))
    x_test = x_test.reshape((len(x_test), np.prod(x_test.shape[1:])))
    print (x_train.shape)
    print (x_test.shape)

    autoencoder.fit(x_train, x_train,
                    epochs=50,
                    batch_size=256,
                    shuffle=True,
                    validation_data=(x_test, x_test))

    autoencoder.save('mod-autoenc-1.h5')
    encoder.save('mod-enc-1.h5')
    decoder.save('mod-dec-1.h5')
