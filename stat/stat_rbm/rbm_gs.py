# En iyi parametreleri bulmak icin grid search yap
from sklearn.cross_validation import train_test_split
from sklearn.metrics import classification_report
from sklearn.base import BaseEstimator
from sklearn.base import TransformerMixin
from sklearn.linear_model import LogisticRegression
from sklearn.neural_network import BernoulliRBM
from sklearn.grid_search import GridSearchCV
from sklearn.pipeline import Pipeline
import argparse, time, rbm
import numpy as np

class SKRBM(BaseEstimator, TransformerMixin):
    """
    Bu sinif bizim RBM kodu ile sklearn arasinda baglantiyi kurmak
    icin yazildi, tek yaptigi parametreleri alip RBM'i cagirmak, bu
    baglanti GridSearch icin gerekli cunku bu altyapi arka planda
    icinde transform() cagrisini yapiyor.
    """
    def __init__(self, n_components=-1, learning_rate=-1, n_iter=-1,num_visible=-1):
      self.n_components = n_components
      self.learning_rate = learning_rate
      self.n_iter = n_iter
      self.num_visible = num_visible
      self.rbm_ = rbm.RBM(num_hidden=self.n_components,
                          learning_rate=self.learning_rate,
                          max_epochs=self.n_iter,num_visible=num_visible)

    def transform(self, X):
      return self.rbm_.run_visible(X)

    def fit(self, X, y=None):
      self.rbm_.fit(X)
      return self


x = np.loadtxt('binarydigits.txt')
labels = np.ravel(np.loadtxt('bindigitlabels.txt'))
X_train, X_test, y_train, y_test = train_test_split(x, labels, test_size=0.4,random_state=0)
print X_train.shape

# initialize the RBM + Logistic Regression pipeline
rbm = SKRBM()
logistic = LogisticRegression()
classifier = Pipeline([("rbm", rbm), ("logistic", logistic)])

# perform a grid search on the learning rate, number of
# iterations, and number of components on the RBM and
# C for Logistic Regression
print "SEARCHING RBM + LOGISTIC REGRESSION"
params = {
"rbm__learning_rate": [0.01, 0.1, 0.2, 0.3, 0.4, 0.5],
"rbm__n_iter": [40, 100, 300, 500],
"rbm__n_components": [10, 20, 30, 40],
"logistic__C": [100.0, 200.0, 400.]}

# perform a grid search over the parameter
start = time.time()
gs = GridSearchCV(classifier, params, n_jobs = -1, verbose = 1)
gs.fit(x, labels)

# print diagnostic information to the user and grab the
# best model
print "\ndone in %0.3fs" % (time.time() - start)
print "best score: %0.3f" % (gs.best_score_)
print "RBM + LOGISTIC REGRESSION PARAMETERS"
bestParams = gs.best_estimator_.get_params()

# loop over the parameters and print each of them out
# so they can be manually set
for p in sorted(params.keys()):
    print "\t %s: %f" % (p, bestParams[p])
