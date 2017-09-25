from tensorflow.contrib import learn
import numpy as np
import data_helpers

nfin = "./data/rt-polarity.pos"
pfin = "./data/rt-polarity.neg"
x_text, y = data_helpers.load_data_and_labels(nfin, pfin)

max_document_length = max([len(x.split(" ")) for x in x_text])
print max_document_length
print x_text[1]
print x_text[2]
print x_text[3]
vocab_processor = learn.preprocessing.VocabularyProcessor(max_document_length)
x = np.array(list(vocab_processor.fit_transform(x_text)))

np.random.seed(10)
shuffle_indices = np.random.permutation(np.arange(len(y)))
x_shuffled = x[shuffle_indices]
y_shuffled = y[shuffle_indices]

dev_sample_index = -1 * int(0.80 * float(len(y)))
x_train, x_dev = x_shuffled[:dev_sample_index], x_shuffled[dev_sample_index:]
y_train, y_dev = y_shuffled[:dev_sample_index], y_shuffled[dev_sample_index:]
print("Vocabulary Size: {:d}".format(len(vocab_processor.vocabulary_)))
print("Train/Dev split: {:d}/{:d}".format(len(y_train), len(y_dev)))
vocabulary_size = len(vocab_processor.vocabulary_)
embedding_size = 400

print('x_train', x_train.shape)
print x_train[19]

import tensorflow as tf
from tensorflow.contrib import learn
from tensorflow.python.framework import ops

ops.reset_default_graph()
sess = tf.Session()

ru = tf.random_uniform([vocabulary_size, embedding_size], -1.0, 1.0)
embeddings = tf.Variable(ru)

A = tf.Variable(tf.random_normal(shape=[embedding_size,1]))
b = tf.Variable(tf.random_normal(shape=[1,1]))

x_data = tf.placeholder(shape=[None, x_train.shape[1]], dtype=tf.int32)
y_target = tf.placeholder(shape=[None, 1], dtype=tf.float32)

embed = tf.nn.embedding_lookup(embeddings, x_data)
embed_avg = tf.reduce_mean(embed, 1)
embed_sum = tf.reduce_sum(embed, 1)
#embedded_chars_expanded = tf.expand_dims(embed, -1)
#print embedded_chars_expanded.shape
#exit()
#model_output = tf.add(tf.matmul(embedded_chars_expanded, A), b)
model_output = tf.add(tf.matmul(embed_avg, A), b)

s = tf.nn.sigmoid_cross_entropy_with_logits(logits=model_output,\
                                            labels=y_target)
loss = tf.reduce_mean(s)
prediction = tf.round(tf.sigmoid(model_output))
predictions_correct = tf.cast(tf.equal(prediction, y_target), tf.float32)
accuracy = tf.reduce_mean(predictions_correct)

my_opt = tf.train.AdagradOptimizer(0.005)
train_step = my_opt.minimize(loss)

init = tf.initialize_all_variables()
sess.run(init)

bsize = 20
epochs = 100
batches = data_helpers.batch_iter(list(zip(x_train, y_train)), bsize, epochs)
batches = data_helpers.batch_iter(list(zip(x_train, y_train)), bsize, epochs)
y_dev = (y_dev[:,1]==1.0).astype(np.float).reshape(len(y_dev),1)
for i,batch in enumerate(batches):
    x_batch, y_batch = zip(*batch)
    x_batch = np.array(x_batch).reshape(len(x_batch),max_document_length)
    y_batch = np.array(y_batch).reshape(len(y_batch),2)
    y_batch = (y_batch[:,1]==1.0).astype(np.float).reshape(len(y_batch),1)
    d = feed_dict={x_data: x_batch, y_target: y_batch}
    train_loss_temp = sess.run(loss, d)
    sess.run(train_step, feed_dict={x_data: x_batch, y_target: y_batch})
    if (i % 10) == 0: 
        train_loss = sess.run(loss, feed_dict={x_data: x_batch, y_target: y_batch})
        dev_loss = sess.run(loss, feed_dict={x_data: x_dev, y_target: y_dev})
        train_acc = sess.run(accuracy, feed_dict={x_data: x_batch, y_target: y_batch})
        dev_acc = sess.run(accuracy, feed_dict={x_data: x_dev, y_target: y_dev})
        print i, 'train loss', train_loss, 'test loss', dev_loss,\
                 'train acc', train_acc, 'test acc', dev_acc
                 



        
