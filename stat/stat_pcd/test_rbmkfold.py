from sklearn.linear_model import LogisticRegression
from sklearn.cross_validation import KFold
import numpy as np, rbmp, sys

X = np.loadtxt('../../stat/stat_mixbern/binarydigits.txt')
Y = np.ravel(np.loadtxt('../../stat/stat_mixbern/bindigitlabels.txt'))

np.random.seed(0)

scores = []
cv = KFold(n=len(X),n_folds=3)
for train, test in cv:
    X_train, Y_train = X[train], Y[train]
    X_test, Y_test = X[test], Y[test]
    r = rbmp.RBM(num_hidden=40, learning_rate=0.1, max_epochs=100,
                 num_visible=64, batch_size=10)
    r.fit(X_train)
    clf = LogisticRegression(C=1000)
    clf.fit(r.run_visible(X_train), Y_train)
    res3 = clf.predict(r.run_visible(X_test))
    scores.append(np.sum(res3==Y_test) / float(len(Y_test)))        
    
print np.mean(scores)
