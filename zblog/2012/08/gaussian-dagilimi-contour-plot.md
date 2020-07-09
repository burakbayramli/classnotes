# Gaussian Dagilimi, Contour Plot




Gaussian Dagilimi, Contour Plot




3 boyutlu Gaussian dagilimi yukaridan cevrit (contour) olarak cizebilen kod parcasi.

# Multivariate gaussian, contours#import scipy.statsimport numpy as npimport matplotlib.pyplot as pltdef norm_pdf(b,mean,cov):   k = b.shape[0]   part1 = np.exp(-0.5*k*np.log(2*np.pi))   part2 = np.power(np.linalg.det(cov),-0.5)   dev = b-mean   part3 = np.exp(-0.5*np.dot(np.dot(dev.transpose(),np.linalg.inv(cov)),dev))   dmvnorm = part1*part2*part3   return dmvnormx = np.arange(55., 80., 1)y = np.arange(80., 280., 1)X, Y = np.meshgrid(x, y)Z = np.zeros(X.shape)nx, ny = X.shapemu = np.array([  65.89350086,  193.21741426])sigma = np.matrix([[    7.84711283,    25.03111826],                    [   25.03111826,  1339.70289046]])for i in xrange(nx):    for j in xrange(ny):        Z[i,j] = norm_pdf(np.array([X[i,j], Y[i,j]]),mu,sigma)        levels = np.linspace(Z.min(), Z.max(), 10)plt.contour(X, Y, Z, colors='b', levels=levels)plt.show()








![](gauss.png)
