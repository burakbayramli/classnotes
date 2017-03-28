import logging, os, time
from collections import defaultdict
from autograd import elementwise_grad
from tqdm import tqdm
from six.moves import range
import autograd.numpy as np
EPS = 1e-15

np.random.seed(9999)

def uniform(shape, scale=0.5):
    return np.random.uniform(size=shape, low=-scale, high=scale)

def logloss(actual, predicted):
    predicted = np.clip(predicted, EPS, 1 - EPS)
    loss = -np.sum(actual * np.log(predicted))
    return loss / float(actual.shape[0])

def convolution_shape(img_height, img_width, filter_shape, stride, padding):
    height = (img_height + 2 * padding[0] - filter_shape[0]) / float(stride[0]) + 1
    width = (img_width + 2 * padding[1] - filter_shape[1]) / float(stride[1]) + 1

    assert height % 1 == 0
    assert width % 1 == 0

    return int(height), int(width)

def pooling_shape(pool_shape, image_shape, stride):
    """Calculate output shape for pooling layer."""
    n_images, n_channels, height, width = image_shape

    height = (height - pool_shape[0]) / float(stride[0]) + 1
    width = (width - pool_shape[1]) / float(stride[1]) + 1

    assert height % 1 == 0
    assert width % 1 == 0

    return int(height), int(width)


def column_to_image(columns, images_shape, filter_shape, stride, padding):
    n_images, n_channels, height, width = images_shape
    f_height, f_width = filter_shape

    out_height, out_width = convolution_shape(height, width,
                                              (f_height, f_width),
                                              stride, padding)
    columns = columns.reshape(n_images, out_height, out_width,
                              n_channels, f_height, f_width).transpose(0,
                                                                       3,
                                                                       4,
                                                                       5,
                                                                       1,
                                                                       2)

    img_h = height + 2 * padding[0] + stride[0] - 1
    img_w = width + 2 * padding[1] + stride[1] - 1
    img = np.zeros((n_images, n_channels, img_h, img_w))
    for y in range(f_height):
        y_bound = y + stride[0] * out_height
        for x in range(f_width):
            x_bound = x + stride[1] * out_width
            img[:,
                :,
                y:y_bound:stride[0],
                x:x_bound:stride[1]] += columns[:, :, y, x, :, :]

    return img[:, :, padding[0]:height + padding[0], padding[1]:width + padding[1]]


categorical_crossentropy = logloss

def _glorot_fan(shape):
    assert len(shape) >= 2

    if len(shape) == 4:
        receptive_field_size = np.prod(shape[2:])
        fan_in = shape[1] * receptive_field_size
        fan_out = shape[0] * receptive_field_size
    else:
        fan_in, fan_out = shape[:2]
    return float(fan_in), float(fan_out)

def glorot_uniform(shape, **kwargs):
    fan_in, fan_out = _glorot_fan(shape)
    s = np.sqrt(6. / (fan_in + fan_out))
    return uniform(shape, s)

def get_initializer(name):
    try:
        return globals()[name]
    except:
        raise ValueError('Invalid initialization function.')

def relu(z):
    return np.maximum(0, z)

def sigmoid(z):
    return 1.0 / (1.0 + np.exp(-z))

def softmax(z):
    e = np.exp(z - np.amax(z, axis=1, keepdims=True))
    return e / np.sum(e, axis=1, keepdims=True)

def unhot(function):
    def wrapper(actual, predicted):
        if len(actual.shape) > 1 and actual.shape[1] > 1:
            actual = actual.argmax(axis=1)
        if len(predicted.shape) > 1 and predicted.shape[1] > 1:
            predicted = predicted.argmax(axis=1)
        return function(actual, predicted)

    return wrapper

def get_activation(name):
    try:
        return globals()[name]
    except:
        raise ValueError('Invalid activation function.')

def batch_iterator(X, batch_size=64):
    n_samples = X.shape[0]
    n_batches = n_samples // batch_size
    batch_end = 0

    for b in range(n_batches):
        batch_begin = b * batch_size
        batch_end = batch_begin + batch_size

        X_batch = X[batch_begin:batch_end]

        yield X_batch

    if n_batches * batch_size < n_samples:
        yield X[batch_end:]

def image_to_column(images, filter_shape, stride, padding):
    n_images, n_channels, height, width = images.shape
    f_height, f_width = filter_shape
    out_height, out_width = convolution_shape(height,
                                              width,
                                              (f_height, f_width),
                                              stride,
                                              padding)
    images = np.pad(images, ((0, 0), (0, 0), padding, padding), mode='constant')

    col = np.zeros((n_images, n_channels, f_height, f_width, out_height, out_width))
    for y in range(f_height):
        y_bound = y + stride[0] * out_height
        for x in range(f_width):
            x_bound = x + stride[1] * out_width
            col[:, :, y, x, :, :] = images[:, :,
                                           y:y_bound:stride[0],
                                           x:x_bound:stride[1]]

    col = col.transpose(0, 4, 5, 1, 2, 3).reshape(n_images * \
                                                  out_height * \
                                                  out_width, -1)
    return col
        
class BaseEstimator(object):
    X = None
    y = None
    y_required = True

    def _setup_input(self, X, y=None):
        if not isinstance(X, np.ndarray):
            X = np.array(X)

        if X.size == 0:
            raise ValueError('Number of features must be > 0')

        if X.ndim == 1:
            self.n_samples, self.n_features = 1, X.shape
        else:
            self.n_samples, self.n_features = X.shape[0], np.prod(X.shape[1:])

        self.X = X

        if self.y_required:
            if y is None:
                raise ValueError('Missed required argument y')

            if not isinstance(y, np.ndarray):
                y = np.array(y)

            if y.size == 0:
                raise ValueError('Number of targets must be > 0')

        self.y = y

    def fit(self, X, y=None):
        self._setup_input(X, y)

    def predict(self, X=None):
        if not isinstance(X, np.ndarray):
            X = np.array(X)

        if self.X is not None:
            return self._predict(X)
        else:
            raise ValueError('You must call `fit` before `predict`')

    def _predict(self, X=None): raise NotImplementedError()

class Layer(object):
    def setup(self, X_shape):
        pass

    def forward_pass(self, x): raise NotImplementedError()

    def backward_pass(self, delta): raise NotImplementedError()

    def shape(self, x_shape): raise NotImplementedError()

class ParamMixin(object):
    @property
    def parameters(self):
        return self._params

    
def get_loss(name):
    try:
        return globals()[name]
    except:
        raise ValueError('Invalid metric function.')

class PhaseMixin(object):
    _train = False

    @property
    def is_training(self):
        return self._train

    @is_training.setter
    def is_training(self, is_train=True):
        self._train = is_train

    @property
    def is_testing(self):
        return not self._train

    @is_testing.setter
    def is_testing(self, is_test=True):
        self._train = not is_test

def get_metric(name):
    try:
        return globals()[name]
    except:
        raise ValueError('Invalid metric function.')


class Optimizer(object):
    def optimize(self, network):
        loss_history = []
        for i in range(network.max_epochs):
            if network.shuffle:
                network.shuffle_dataset()
            start_time = time.time()
            loss = self.train_epoch(network)
            loss_history.append(loss)
            msg = "Epoch:%s, train loss: %s" % (i, loss)
            if network.log_metric:
                msg += ', train %s: %s' % (network.metric_name, network.error())
            msg += ', elapsed: %s sec.' % (time.time() - start_time)
            logging.info(msg)
        return loss_history

    def update(self, network): raise NotImplementedError

    def train_epoch(self, network):
        self._setup(network)
        losses = []

        X_batch = batch_iterator(network.X, network.batch_size)
        y_batch = batch_iterator(network.y, network.batch_size)
        for X, y in tqdm(zip(X_batch, y_batch), 'Epoch progress'):
            loss = np.mean(network.update(X, y))
            self.update(network)
            losses.append(loss)
        epoch_loss = np.mean(losses)
        return epoch_loss

    def _setup(self, network): raise NotImplementedError


@unhot
def accuracy(actual, predicted):
    return 1.0 - classification_error(actual, predicted)

@unhot
def classification_error(actual, predicted):
    return (actual != predicted).sum() / float(actual.shape[0])


class NeuralNet(BaseEstimator):
    def __init__(self, layers, optimizer, loss,
                 max_epochs=10, batch_size=64,
                 random_seed=33, metric='mse',
                 shuffle=True):
        self.shuffle = shuffle
        self.optimizer = optimizer
        self.loss = get_loss(loss)

        if loss == 'categorical_crossentropy':
            self.loss_grad = lambda actual, predicted: -(actual - predicted)
        else:
            self.loss_grad = elementwise_grad(self.loss, 1)
        self.metric = get_metric(metric)
        self.random_seed = random_seed
        self.layers = layers
        self.batch_size = batch_size
        self.max_epochs = max_epochs
        self._n_layers = 0
        self.log_metric = True if loss != metric else False
        self.metric_name = metric
        self.bprop_entry = self._find_bprop_entry()
        self.training = False
        self._initialized = False

    def _setup_layers(self, x_shape, ):
        x_shape = list(x_shape)
        x_shape[0] = self.batch_size

        for layer in self.layers:
            layer.setup(x_shape)
            x_shape = layer.shape(x_shape)

        self._n_layers = len(self.layers)
        self._initialized = True
        logging.info('Total parameters: %s' % self.n_params)

    def _find_bprop_entry(self):
        if len(self.layers) > 0 and not hasattr(self.layers[-1], 'parameters'):
            return -1
        return len(self.layers)

    def fit(self, X, y=None):
        if y.ndim == 1:
            y = y[:, np.newaxis]
        self._setup_input(X, y)
        if not self._initialized:
            self._setup_layers(X.shape)
        self.is_training = True
        self.optimizer.optimize(self)
        self.is_training = False

    def update(self, X, y):
        y_pred = self.fprop(X)
        grad = self.loss_grad(y, y_pred)
        for layer in reversed(self.layers[:self.bprop_entry]):
            grad = layer.backward_pass(grad)
        return self.loss(y, y_pred)

    def fprop(self, X):
        for layer in self.layers:
            X = layer.forward_pass(X)
        return X

    def _predict(self, X=None):
        y = []
        X_batch = batch_iterator(X, self.batch_size)
        for Xb in X_batch:
            y.append(self.fprop(Xb))
        return np.concatenate(y)

    @property
    def parametric_layers(self):
        for layer in self.layers:
            if hasattr(layer, 'parameters'):
                yield layer

    @property
    def parameters(self):
        params = []
        for layer in self.parametric_layers:
            params.append(layer.parameters)
        return params

    def error(self, X=None, y=None):
        training_phase = self.is_training
        if training_phase:
            self.is_training = False
        if X is None and y is None:
            y_pred = self._predict(self.X)
            score = self.metric(self.y, y_pred)
        else:
            y_pred = self._predict(X)
            score = self.metric(y, y_pred)
        if training_phase:
            self.is_training = True
        return score

    @property
    def is_training(self):
        return self.training

    @is_training.setter
    def is_training(self, train):
        self.training = train
        for layer in self.layers:
            if isinstance(layer, PhaseMixin):
                layer.is_training = train

    def shuffle_dataset(self):
        n_samples = self.X.shape[0]
        indices = np.arange(n_samples)
        np.random.shuffle(indices)
        self.X = self.X.take(indices, axis=0)
        self.y = self.y.take(indices, axis=0)

    @property
    def n_layers(self):
        return self._n_layers

    @property
    def n_params(self):
        return sum([layer.parameters.n_params for layer in self.parametric_layers])

    def reset(self):
        self._initialized = False

class MaxPooling(Layer):
    def __init__(self, pool_shape=(2, 2), stride=(1, 1), padding=(0, 0)):
        self.pool_shape = pool_shape
        self.stride = stride
        self.padding = padding

    def forward_pass(self, X):
        self.last_input = X

        out_height, out_width = pooling_shape(self.pool_shape,
                                              X.shape,
                                              self.stride)
        n_images, n_channels, _, _ = X.shape

        col = image_to_column(X, self.pool_shape, self.stride, self.padding)
        col = col.reshape(-1, self.pool_shape[0] * self.pool_shape[1])

        arg_max = np.argmax(col, axis=1)
        out = np.max(col, axis=1)
        self.arg_max = arg_max
        return out.reshape(n_images,
                           out_height,
                           out_width,
                           n_channels).transpose(0, 3, 1, 2)

    def backward_pass(self, delta):
        delta = delta.transpose(0, 2, 3, 1)

        pool_size = self.pool_shape[0] * self.pool_shape[1]
        y_max = np.zeros((delta.size, pool_size))
        y_max[np.arange(self.arg_max.size), self.arg_max.flatten()] = delta.flatten()
        y_max = y_max.reshape(delta.shape + (pool_size,))

        dcol = y_max.reshape(y_max.shape[0] * y_max.shape[1] * y_max.shape[2], -1)
        return column_to_image(dcol,
                               self.last_input.shape,
                               self.pool_shape,
                               self.stride,
                               self.padding)

    def shape(self, x_shape):
        h, w = convolution_shape(x_shape[2],
                                 x_shape[3],
                                 self.pool_shape,
                                 self.stride,
                                 self.padding)
        return x_shape[0], x_shape[1], h, w

class Flatten(Layer):
    def forward_pass(self, X):
        self.last_input_shape = X.shape
        return X.reshape((X.shape[0], -1))

    def backward_pass(self, delta):
        return delta.reshape(self.last_input_shape)

    def shape(self, x_shape):
        return x_shape[0], np.prod(x_shape[1:])


class Parameters(object):
    def __init__(self, init='glorot_uniform',
                 scale=0.5, bias=1.0,
                 regularizers=None, constraints=None):
        if constraints is None:
            self.constraints = {}
        else:
            self.constraints = constraints

        if regularizers is None:
            self.regularizers = {}
        else:
            self.regularizers = regularizers

        self.initial_bias = bias
        self.scale = scale
        self.init = get_initializer(init)

        self._params = {}
        self._grads = {}

    def setup_weights(self, W_shape, b_shape=None):
        if 'W' not in self._params:
            self._params['W'] = self.init(shape=W_shape, scale=self.scale)
            if b_shape is None:
                self._params['b'] = np.full(W_shape[1], self.initial_bias)
            else:
                self._params['b'] = np.full(b_shape, self.initial_bias)
        self.init_grad()

    def init_grad(self):
        for key in self._params.keys():
            if key not in self._grads:
                self._grads[key] = np.zeros_like(self._params[key])

    def step(self, name, step):
        self._params[name] += step

        if name in self.constraints:
            self._params[name] = self.constraints[name].clip(self._params[name])

    def update_grad(self, name, value):
        self._grads[name] = value

        if name in self.regularizers:
            self._grads[name] += self.regularizers[name](self._params[name])

    @property
    def n_params(self):
        return sum([np.prod(self._params[x].shape) for x in self._params.keys()])

    def keys(self):
        return self._params.keys()

    @property
    def grad(self):
        return self._grads

    def __getitem__(self, item):
        if item in self._params:
            return self._params[item]
        else:
            raise ValueError

    def __setitem__(self, key, value):
        self._params[key] = value

        
class Convolution(Layer, ParamMixin):
    def __init__(self, n_filters=8,
                 filter_shape=(3, 3),
                 padding=(0, 0),
                 stride=(1, 1),
                 parameters=None):
        
        self.padding = padding
        self._params = parameters
        self.stride = stride
        self.filter_shape = filter_shape
        self.n_filters = n_filters
        if self._params is None:
            self._params = Parameters()

    def setup(self, X_shape):
        n_channels, self.height, self.width = X_shape[1:]

        W_shape = (self.n_filters, n_channels) + self.filter_shape
        b_shape = (self.n_filters)
        self._params.setup_weights(W_shape, b_shape)

    def forward_pass(self, X):
        n_images, n_channels, height, width = self.shape(X.shape)
        self.last_input = X
        self.col = image_to_column(X, self.filter_shape, self.stride, self.padding)
        self.col_W = self._params['W'].reshape(self.n_filters, -1).T

        out = np.dot(self.col, self.col_W) + self._params['b']
        out = out.reshape(n_images, height, width, -1).transpose(0, 3, 1, 2)
        return out

    def backward_pass(self, delta):
        delta = delta.transpose(0, 2, 3, 1).reshape(-1, self.n_filters)

        d_W = np.dot(self.col.T, delta).transpose(1, 0).reshape(self._params['W'].shape)
        d_b = np.sum(delta, axis=0)
        self._params.update_grad('b', d_b)
        self._params.update_grad('W', d_W)

        d_c = np.dot(delta, self.col_W.T)
        return column_to_image(d_c,
                               self.last_input.shape,
                               self.filter_shape,
                               self.stride,
                               self.padding)

    def shape(self, x_shape):
        height, width = convolution_shape(self.height,
                                          self.width,
                                          self.filter_shape,
                                          self.stride,
                                          self.padding)
        return x_shape[0], self.n_filters, height, width


class Dropout(Layer, PhaseMixin):
    def __init__(self, p=0.1):
        self.p = p
        self._mask = None

    def forward_pass(self, X):
        assert self.p > 0
        if self.is_training:
            self._mask = np.random.uniform(size=X.shape) > self.p
            y = X * self._mask
        else:
            y = X * (1.0 - self.p)

        return y

    def backward_pass(self, delta):
        return delta * self._mask

    def shape(self, x_shape):
        return x_shape


class Activation(Layer):
    def __init__(self, name):
        self.last_input = None
        self.activation = get_activation(name)
        self.activation_d = elementwise_grad(self.activation)

    def forward_pass(self, X):
        self.last_input = X
        return self.activation(X)

    def backward_pass(self, delta):
        return self.activation_d(self.last_input) * delta

    def shape(self, x_shape):
        return x_shape


class Dense(Layer, ParamMixin):
    def __init__(self, output_dim, parameters=None, ):
        self._params = parameters
        self.output_dim = output_dim
        self.last_input = None
        if parameters is None:
            self._params = Parameters()

    def setup(self, x_shape):
        self._params.setup_weights((x_shape[1], self.output_dim))

    def forward_pass(self, X):
        self.last_input = X
        return self.weight(X)

    def weight(self, X):
        W = np.dot(X, self._params['W'])
        return W + self._params['b']

    def backward_pass(self, delta):
        dW = np.dot(self.last_input.T, delta)
        db = np.sum(delta, axis=0)
        self._params.update_grad('W', dW)
        self._params.update_grad('b', db)
        return np.dot(delta, self._params['W'].T)

    def shape(self, x_shape):
        return x_shape[0], self.output_dim

class Adadelta(Optimizer):
    def __init__(self, learning_rate=1.0, rho=0.95, epsilon=1e-8):
        self.rho = rho
        self.eps = epsilon
        self.lr = learning_rate

    def update(self, network):
        for i, layer in enumerate(network.parametric_layers):
            for n in layer.parameters.keys():
                grad = layer.parameters.grad[n]
                self.accu[i][n] = self.rho * \
                                  self.accu[i][n] + (1. - self.rho) * \
                                  grad ** 2
                step = grad * np.sqrt(self.d_accu[i][n] + self.eps) / np.sqrt(
                    self.accu[i][n] + self.eps)

                layer.parameters.step(n, -step * self.lr)
                self.d_accu[i][n] = self.rho * \
                                    self.d_accu[i][n] + \
                                    (1. - self.rho) * step ** 2

    def _setup(self, network):
        self.accu = defaultdict(dict)
        self.d_accu = defaultdict(dict)
        for i, layer in enumerate(network.parametric_layers):
            for n in layer.parameters.keys():
                self.accu[i][n] = np.zeros_like(layer.parameters[n])
                self.d_accu[i][n] = np.zeros_like(layer.parameters[n])


if __name__ == "__main__":

    X = np.loadtxt('../../stat/stat_mixbern/binarydigits.txt')
    y = np.ravel(np.loadtxt('../../stat/stat_mixbern/bindigitlabels.txt'))
    X = np.reshape(X, (100,1,8,8))
    y = np.reshape(y,(100,1)).astype(int)
    y = one_hot(y.flatten())
    X_train3 = X[:90]; y_train3 = y[:90]
    X_test3 = X[90:]; y_test3 = y[90:]
    print(X_train3.shape, X_test3.shape, y_train3.shape, y_test3.shape)
    
    model2 = convnet.NeuralNet(
        layers=[
            convnet.Convolution(n_filters=32, filter_shape=(3, 3), padding=(1, 1), stride=(1, 1)),
            convnet.Activation('relu'),
            convnet.Convolution(n_filters=32, filter_shape=(3, 3), padding=(1, 1), stride=(1, 1)),
            convnet.Activation('relu'),
            convnet.MaxPooling(pool_shape=(2, 2), stride=(2, 2)),
            convnet.Dropout(0.1),
            convnet.Flatten(),
            convnet.Dense(100),
            convnet.Activation('relu'),
            convnet.Dropout(0.1),
            convnet.Dense(8),
            convnet.Activation('softmax'),
        ],
        loss='categorical_crossentropy',
        optimizer=convnet.Adadelta(),
        metric='accuracy',
        batch_size=3,
        max_epochs=10,
    )

    model2.fit(X_train3, y_train3)
    predictions = model2.predict(X_test3)
    print(convnet.accuracy(y_test3, predictions))        
