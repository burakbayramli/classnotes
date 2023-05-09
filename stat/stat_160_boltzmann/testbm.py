from sklearn import neighbors
import numpy as np, boltz
from sklearn.cross_validation import train_test_split

Y = np.loadtxt('../../stat/stat_mixbern/binarydigits.txt')
labels = np.ravel(np.loadtxt('../../stat/stat_mixbern/bindigitlabels.txt'))
X_train, X_test, y_train, y_test = train_test_split(Y, labels, test_size=0.4,random_state=0)
X_train[X_train==0]=-1
X_test[X_test==0]=-1

clfs = {}
for label in [0,5,7]:
    x = X_train[y_train==label]
    clf = boltz.Boltzmann(n_iter=30,eta=0.05,sample_size=500,init_sample_size=100)
    clf.fit(x)
    clfs[label] = clf
    
res = []
for label in [0,5,7]:
    res.append(clfs[label].predict_proba(X_test))
    
res3 = np.argmax(np.array(res).T,axis=1)
res3[res3==1] = 5
res3[res3==2] = 7
print 'Boltzmann Makinasi', np.sum(res3==y_test) / float(len(y_test))

clf = neighbors.KNeighborsClassifier()
clf.fit(X_train,y_train)
res3 = clf.predict(X_test)    
print 'KNN', np.sum(res3==y_test) / float(len(y_test))
