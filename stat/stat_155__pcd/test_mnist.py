import numpy as np, gzip, sys
from sklearn import neighbors
from sklearn.cross_validation import train_test_split
from sklearn.linear_model import LogisticRegression

np.random.seed(0)
S = 1000

f = gzip.open('/tmp/mnist.pkl.gz', 'rb')
train_set, valid_set, test_set = cPickle.load(f)
f.close()

X_train,y_train = train_set
X_test,y_test = valid_set
X_train = X_train[:S]; y_train = y_train[:S]
X_test = X_test[:S]; y_test = y_test[:S]
print X_train.shape

clf = neighbors.KNeighborsClassifier(n_neighbors=1)
clf.fit(X_train, y_train)
print 'KNN', clf.score(X_test, y_test)

import rbmp
r = rbmp.RBM(num_hidden=500, learning_rate=0.1, max_epochs=200,
             num_visible=784,batch_size=20)
r.fit(X_train)
clf = LogisticRegression(C=1000)
clf.fit(r.run_visible(X_train), y_train)
res3 = clf.predict(r.run_visible(X_test))
print 'RBM', np.sum(res3==y_test) / float(len(y_test))
