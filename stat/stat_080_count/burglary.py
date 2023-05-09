import numpy as np
import scikits.statsmodels as sm

data = np.loadtxt("burglary.txt",  skiprows=1, usecols = (1,2))

exog = data[:,1]
endog = data[:,0]

endog1 = endog[endog > 0]
exog1 = exog[endog > 0]

exog1 = sm.add_constant(exog1, prepend=True)

glm = sm.GLM(endog1, exog1, family=sm.family.Poisson())
res = glm.fit()

print "res.deviance=" + str(res.deviance)
print "res.scale=" + str(res.scale)
print "res.params=" + str(res.params)
print "res.pearson_chi2=" + str(res.pearson_chi2)
print "res.df_model=" + str(res.df_model)
print "res.null_deviance=" + str(res.null_deviance)
print "res.t()=" + str(res.t())
print "\n"

exog = sm.add_constant(exog, prepend=True)

glm = sm.GLM(endog, exog, family=sm.family.NegativeBinomial())
res = glm.fit()

print "res.deviance=" + str(res.deviance)
print "res.scale=" + str(res.scale)
print "res.params=" + str(res.params)
print "res.pearson_chi2=" + str(res.pearson_chi2)
print "res.df_model=" + str(res.df_model)
print "res.null_deviance=" + str(res.null_deviance)
print "res.t()=" + str(res.t())

