import numpy as np, os
import convnet, logging

# MNIST verisi https://github.com/rushter/MLAlgorithms projesinde
# mla/datasets/data/mnist altinda.

def one_hot(y):
    n_values = np.max(y) + 1
    return np.eye(n_values)[y]

def get_filename(name):
    return os.path.join(os.path.dirname(__file__), name)

def load_mnist():
    def load(dataset="training", digits=np.arange(10)):
        import struct
        from array import array as pyarray
        from numpy import array, int8, uint8, zeros

        if dataset == "train":
            fname_img = '/home/burak/Downloads/train-images-idx3-ubyte'
            fname_lbl = '/home/burak/Downloads/train-labels-idx1-ubyte'
        elif dataset == "test":
            fname_img = '/home/burak/Downloads/t10k-images-idx3-ubyte'
            fname_lbl = '/home/burak/Downloads/t10k-labels-idx1-ubyte'

        flbl = open(fname_lbl, 'rb')
        magic_nr, size = struct.unpack(">II", flbl.read(8))
        lbl = pyarray("b", flbl.read())
        flbl.close()

        fimg = open(fname_img, 'rb')
        magic_nr, size, rows, cols = struct.unpack(">IIII", fimg.read(16))
        img = pyarray("B", fimg.read())
        fimg.close()

        ind = [k for k in range(size) if lbl[k] in digits]
        N = len(ind)

        images = zeros((N, rows, cols), dtype=uint8)
        labels = zeros((N, 1), dtype=int8)
        for i in range(len(ind)):
            images[i] = array(img[ind[i] * rows * cols: (ind[i] + 1) * rows * cols]).reshape((rows, cols))
            labels[i] = lbl[ind[i]]

        return images, labels

    X_train, y_train = load('train')
    X_test, y_test = load('test')

    X_train = X_train.reshape(X_train.shape[0], 1, 28, 28).astype(np.float32)
    X_test = X_test.reshape(X_test.shape[0], 1, 28, 28).astype(np.float32)

    return X_train, X_test, y_train, y_test

logging.basicConfig(level=logging.DEBUG)
X_train, X_test, y_train, y_test = load_mnist()

X_train /= 255.; X_test /= 255.

y_train = one_hot(y_train.flatten())
y_test = one_hot(y_test.flatten())
print(X_train.shape, X_test.shape, y_train.shape, y_test.shape)

N = 300
idxtrain = np.random.randint(0,len(X_train),N)
idxtest = np.random.randint(0,len(X_test),N/2)
X_train2 = X_train[idxtrain]; y_train2 = y_train[idxtrain]
X_test2 = X_test[idxtest]; y_test2 = y_test[idxtest]
print(X_train2.shape, X_test2.shape, y_train2.shape, y_test2.shape)

model = convnet.NeuralNet(
    layers=[
        convnet.Convolution(n_filters=32, filter_shape=(3, 3), padding=(1, 1), stride=(1, 1)),
        convnet.Activation('relu'),
        convnet.Convolution(n_filters=32, filter_shape=(3, 3), padding=(1, 1), stride=(1, 1)),
        convnet.Activation('relu'),
        convnet.MaxPooling(pool_shape=(2, 2), stride=(2, 2)),
        convnet.Dropout(0.2),
        convnet.Flatten(),
        convnet.Dense(100),
        convnet.Activation('relu'),
        convnet.Dropout(0.1),
        convnet.Dense(10),
        convnet.Activation('softmax'),
    ],
    loss='categorical_crossentropy',
    optimizer=convnet.Adadelta(),
    metric='accuracy',
    batch_size=3,
    max_epochs=10,
)

model.fit(X_train2, y_train2)
predictions = model.predict(X_test2)
print(convnet.accuracy(y_test2, predictions))       
