# Keras


Keras




Arka planda TensorFlow ya da Theano kullanabilen bir yapay sinir ağı paketi Keras. Python için yazılmıştır ve YSA kodlamasını çok rahatlatır. En basit program (arka planda theano motoru kullanılması şart değil, çoğunlukla Tensorflow kullanılır, bu durumda üstten 2. satıra gerek yoktur)

from __future__ import print_function
import os; os.environ['KERAS_BACKEND'] = 'theano'
import keras
from keras.datasets import mnist
from keras.models import Sequential
from keras.layers import Dense, Dropout
from keras.optimizers import RMSprop

batch_size = 128
num_classes = 10
epochs = 20

# the data, split between train and test sets
(x_train, y_train), (x_test, y_test) = mnist.load_data()

x_train = x_train.reshape(60000, 784)
x_test = x_test.reshape(10000, 784)
x_train = x_train.astype('float32')
x_test = x_test.astype('float32')
x_train /= 255
x_test /= 255
print(x_train.shape[0], 'train samples')
print(x_test.shape[0], 'test samples')

# convert class vectors to binary class matrices
y_train = keras.utils.to_categorical(y_train, num_classes)
y_test = keras.utils.to_categorical(y_test, num_classes)

model = Sequential()
model.add(Dense(512, activation='relu', input_shape=(784,)))
model.add(Dropout(0.2))
model.add(Dense(512, activation='relu'))
model.add(Dropout(0.2))
model.add(Dense(num_classes, activation='softmax'))

model.summary()

model.compile(loss='categorical_crossentropy',
              optimizer=RMSprop(),
              metrics=['accuracy'])

history = model.fit(x_train, y_train,
                    batch_size=batch_size,
                    epochs=epochs,
                    verbose=1,
                    validation_data=(x_test, y_test))
score = model.evaluate(x_test, y_test, verbose=0)
print('Test loss:', score[0])
print('Test accuracy:', score[1])


Model kaydetmek

Egitilmis bir modeli kaydetmek icin

model.save("model.h5")

Model model.h5 altinda kaydedilmis olacak. Yuklemek icin ustte goruldugu gibi model tanimi yapildiktan sonra (bu sart, YSA ag yapisi bilinmeli ki ona gore agirliklar yuklenebilsin),

model.load("model.h5")







