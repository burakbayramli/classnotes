# nlp1.py
import tensorflow as tf
import numpy as np
import data_helpers
from tensorflow.contrib import learn

dev_sample_percentage = .1
positive_data_file = "./data/rt-polarity.pos"
negative_data_file = "./data/rt-polarity.neg"
embedding_dim = 120
batch_size = 40
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

input_x = tf.placeholder(tf.int32, [None, sequence_length])
input_y = tf.placeholder(tf.float32, [None, num_classes])

# rasgele agirliklar
W = tf.Variable(tf.random_uniform([len(vocab_processor.vocabulary_),
                                   embedding_dim], -1.0, 1.0))

ec = tf.nn.embedding_lookup(W, input_x)

# duzlestir
embed = tf.contrib.layers.flatten(ec)

scores = tf.contrib.layers.fully_connected(inputs=embed, num_outputs=2, 
                                           activation_fn=tf.nn.softmax)

predictions = tf.argmax(scores, 1)

losses = tf.nn.softmax_cross_entropy_with_logits(logits=scores, labels=input_y)

loss = tf.reduce_mean(losses) 

correct_predictions = tf.equal(predictions, tf.argmax(input_y, 1))

accuracy = tf.reduce_mean(tf.cast(correct_predictions, "float"))

global_step = tf.Variable(0, trainable=False)

optimizer = tf.train.AdamOptimizer(1e-3)

grads_and_vars = optimizer.compute_gradients(loss)

train_op = optimizer.apply_gradients(grads_and_vars, global_step=global_step)

sess = tf.Session()

sess.run(tf.global_variables_initializer())

batches = data_helpers.batch_iter(list(zip(x_train, y_train)),batch_size,num_epochs)

saver = tf.train.Saver(tf.global_variables())

for i,batch in enumerate(batches):
    
    x_batch, y_batch = zip(*batch)    
    feed_dict = { input_x: x_batch, input_y: y_batch }    
    sess.run(train_op, feed_dict)
    
    if (i % 30) == 0:
        feed_dict2 = { input_x: x_dev, input_y: y_dev }
        train_acc = sess.run(accuracy, feed_dict)
        test_acc = sess.run(accuracy, feed_dict2)
        print train_acc, test_acc
    if (i % 200) == 0: # arada sirada modeli kaydet
        path = saver.save(sess, "/tmp/nlpembed1")
