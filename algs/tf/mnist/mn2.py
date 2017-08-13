import pandas as pd
import numpy as np
from keras.datasets import mnist
from keras.utils.np_utils import to_categorical
from keras.layers.convolutional import *
import matplotlib.pyplot as plt
from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation, Flatten


## Load the data ##
(X_train, y_train), (X_test, y_test) = mnist.load_data()

## Set the number of training samples to use ##
batch_size = 1

X_train = np.expand_dims(X_train, axis=3)
X_test = np.expand_dims (X_test, axis=3)

# Get training data for 0 and 1
inds = np.where((y_train == 0) | (y_train == 1))[0]
train_size = 80
X_train_small = X_train[inds]
X_train_small = X_train_small[:train_size]
y_train_small = y_train[inds]
y_train_small = y_train_small[:train_size]

# Get test data for 0 and 1 only
inds_test = np.where((y_test == 0) | (y_test == 1))
X_test_01 = X_test[inds_test]
y_test_01 = y_test[inds_test]

# split into validation and test sets
test_start_ind = int(np.floor((len(X_test_01)/2)))
X_test_small = X_test_01[:test_start_ind]
X_final_test = X_test_01[test_start_ind:]

y_test_small = y_test_01[:test_start_ind]
y_final_test = y_test_01[test_start_ind:]

# one hot labels
one_hot_train = to_categorical(y_train_small, 2)
one_hot_test = to_categorical(y_test_small, 2)
one_hot_final_test = to_categorical(y_final_test, 2)

n_classes = one_hot_train.shape[1]

index = 0
for i in range(5):
    fold_inds = np.random.choice(inds,train_size)
    X_train_fold = X_train[fold_inds]
    y_train_fold = y_train[fold_inds]
    one_hot_fold = to_categorical(y_train_fold, 2)
    print X_train_fold.shape, one_hot_fold.shape

X_test_flat = X_test_small.reshape((len(X_test_small),784))
X_final_test_flat = X_final_test.reshape((len(X_final_test),784))

print X_test_flat.shape
print X_final_test_flat.shape

