# nlp2.py
import tensorflow as tf
import numpy as np
import data_helpers
from tensorflow.contrib import learn

dev_sample_percentage = .1
positive_data_file = "./data/rt-polarity.pos"
negative_data_file = "./data/rt-polarity.neg"
embedding_dim = 200
filter_sizes = "3,4,5"
num_filters = 200
dropout_keep_prob = 0.5
l2_reg_lambda = 0.0
batch_size = 70
num_epochs = 200

x_text, y = data_helpers.load_data_and_labels(positive_data_file, negative_data_file)

max_document_length = max([len(x.split(" ")) for x in x_text])
vocab_processor = learn.preprocessing.VocabularyProcessor(max_document_length)
x = np.array(list(vocab_processor.fit_transform(x_text)))

np.random.seed(10)
shuffle_indices = np.random.permutation(np.arange(len(y)))
x_shuffled = x[shuffle_indices]
y_shuffled = y[shuffle_indices]

dev_sample_index = -1 * int(dev_sample_percentage * float(len(y)))
x_train, x_dev = x_shuffled[:dev_sample_index], x_shuffled[dev_sample_index:]
y_train, y_dev = y_shuffled[:dev_sample_index], y_shuffled[dev_sample_index:]
print("Vocabulary Size: {:d}".format(len(vocab_processor.vocabulary_)))
print("Train/Dev split: {:d}/{:d}".format(len(y_train), len(y_dev)))

tf.reset_default_graph()

num_classes=y_train.shape[1]
sequence_length=x_train.shape[1]
filter_sizes=list(map(int, filter_sizes.split(",")))

input_x = tf.placeholder(tf.int32, [None, sequence_length])
input_y = tf.placeholder(tf.float32, [None, num_classes])
dropout_keep_prob = tf.placeholder(tf.float32)

l2_loss = tf.constant(0.0)

W = tf.Variable(tf.random_uniform([len(vocab_processor.vocabulary_),
                                   embedding_dim], -1.0, 1.0))

embedded_chars = tf.nn.embedding_lookup(W, input_x)
embedded_chars_expanded = tf.expand_dims(embedded_chars, -1)

pooled_outputs = []
for i, filter_size in enumerate(filter_sizes):
    filter_shape = [filter_size, embedding_dim, 1, num_filters]
    W = tf.Variable(tf.truncated_normal(filter_shape, stddev=0.1))
    b = tf.Variable(tf.constant(0.1, shape=[num_filters]))
    conv = tf.nn.conv2d(
        embedded_chars_expanded,
        W,
        strides=[1, 1, 1, 1],
        padding="VALID")

    h = tf.nn.relu(tf.nn.bias_add(conv, b))

    pooled = tf.nn.max_pool(
        h, ksize=[1, sequence_length - filter_size + 1, 1, 1],
        strides=[1, 1, 1, 1],
        padding='VALID',
        name="pool")
    pooled_outputs.append(pooled)

num_filters_total = num_filters * len(filter_sizes)

h_pool = tf.concat(pooled_outputs, 3)

h_pool_flat = tf.reshape(h_pool, [-1, num_filters_total])

h_drop = tf.nn.dropout(h_pool_flat, dropout_keep_prob)

l2_loss = tf.constant(0.0)

W = tf.Variable(tf.random_normal(shape=[num_filters_total, num_classes]))

b = tf.Variable(tf.constant(0.1, shape=[num_classes]))

l2_loss += tf.nn.l2_loss(W)
l2_loss += tf.nn.l2_loss(b)

scores = tf.nn.xw_plus_b(h_drop, W, b)

predictions = tf.argmax(scores, 1)

losses = tf.nn.softmax_cross_entropy_with_logits(logits=scores, labels=input_y)

loss = tf.reduce_mean(losses) + l2_reg_lambda * l2_loss

correct_predictions = tf.equal(predictions, tf.argmax(input_y, 1))

accuracy = tf.reduce_mean(tf.cast(correct_predictions, "float"))

global_step = tf.Variable(0, trainable=False)

optimizer = tf.train.AdamOptimizer(1e-3)

grads_and_vars = optimizer.compute_gradients(loss)

train_op = optimizer.apply_gradients(grads_and_vars, global_step=global_step)

sess = tf.Session()

sess.run(tf.global_variables_initializer())

batches = data_helpers.batch_iter(
    list(zip(x_train, y_train)), batch_size, num_epochs)

saver = tf.train.Saver(tf.global_variables())

for i,batch in enumerate(batches):
    x_batch, y_batch = zip(*batch)
    feed_dict = {
        input_x: x_batch,input_y: y_batch, dropout_keep_prob: dropout_kp
    }
    sess.run(train_op, feed_dict)
    if (i % 30) == 0:
        feed_dict2 = {
            input_x: x_dev,
            input_y: y_dev,
            dropout_keep_prob: dropout_kp
        }
        train_acc = sess.run(accuracy, feed_dict)
        test_acc = sess.run(accuracy, feed_dict2)
        print train_acc, test_acc
    if (i % 100) == 0:
        path = saver.save(sess, "/tmp/nlpembed2")


