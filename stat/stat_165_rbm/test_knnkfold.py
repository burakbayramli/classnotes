from sklearn import neighbors
from sklearn.linear_model import LogisticRegression
import numpy as np

X = np.loadtxt('../../stat/stat_mixbern/binarydigits.txt')
Y = np.ravel(np.loadtxt('../../stat/stat_mixbern/bindigitlabels.txt'))

from sklearn.cross_validation import KFold
scores = []
cv = KFold(n=len(X),n_folds=3)
for train, test in cv:
    X_train, Y_train = X[train], Y[train]
    X_test, Y_test = X[test], Y[test]
    clf = neighbors.KNeighborsClassifier(n_neighbors=1)
    clf.fit(X_train, Y_train)
    scores.append(clf.score(X_test, Y_test))
    
print np.mean(scores)
            
