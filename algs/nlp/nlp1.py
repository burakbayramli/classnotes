# nlp1.py
import tensorflow as tf
import numpy as np
import data_helpers
from tensorflow.contrib import learn
import constants

FLAGS = tf.flags.FLAGS
FLAGS._parse_flags()
print("\nParameters:")
for attr, value in sorted(FLAGS.__flags.items()):
    print("{}={}".format(attr.upper(), value))
print("")

print("Loading data...")
x_text, y = data_helpers.load_data_and_labels(FLAGS.positive_data_file, FLAGS.negative_data_file)

max_document_length = max([len(x.split(" ")) for x in x_text])
vocab_processor = learn.preprocessing.VocabularyProcessor(max_document_length)
x = np.array(list(vocab_processor.fit_transform(x_text)))

np.random.seed(10)
shuffle_indices = np.random.permutation(np.arange(len(y)))
x_shuffled = x[shuffle_indices]
y_shuffled = y[shuffle_indices]

dev_sample_index = -1 * int(FLAGS.dev_sample_percentage * float(len(y)))
x_train, x_dev = x_shuffled[:dev_sample_index], x_shuffled[dev_sample_index:]
y_train, y_dev = y_shuffled[:dev_sample_index], y_shuffled[dev_sample_index:]
print("Vocabulary Size: {:d}".format(len(vocab_processor.vocabulary_)))
print("Train/Dev split: {:d}/{:d}".format(len(y_train), len(y_dev)))

tf.reset_default_graph()

num_classes=y_train.shape[1]
sequence_length=x_train.shape[1]
num_filters=FLAGS.num_filters
filter_sizes=list(map(int, FLAGS.filter_sizes.split(",")))
l2_reg_lambda=0.0

input_x = tf.placeholder(tf.int32, [None, sequence_length])
input_y = tf.placeholder(tf.float32, [None, num_classes])

W = tf.Variable(tf.random_uniform([len(vocab_processor.vocabulary_),
                                   FLAGS.embedding_dim], -1.0, 1.0))

embedded_chars = tf.nn.embedding_lookup(W, input_x)
embedded_chars_expanded = tf.expand_dims(embedded_chars, -1)

prediction = tf.contrib.layers.fully_connected(inputs=embedded_chars_expanded,
                                               num_outputs=2, 
                                               activation_fn=tf.nn.softmax)

cost = tf.losses.softmax_cross_entropy(onehot_labels=y,logits=prediction)

optimizer = tf.train.AdagradOptimizer(0.01).minimize(cost)


        
