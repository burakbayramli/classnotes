# 2 Boyutlu 2 Gaussian Karisimi, 3D




2 Boyutlu 2 Gaussian Karisimi, 3D



Iki boyutlu iki Gaussian karisimini alttaki kodlarla hesaplayip grafikliyoruz. Bazi notlar: scipy.stats.norm.pdf cagrisi tek bir skalar olasilik degeri dondurmesi gerekirken, bir matris geriye  dondurdu. Bu matris belki ek islemden gecerek dogru sonu elde edilebilirdi, fakat pdf cagrisindan beklenen bu degildir. Bu sebeple pdf kodunu kendimiz yazdik.Ayrica grafiklerken countour() cagrisi tek Gaussian ile calisti, fakat iki Gaussian problem cikardi. Bu sebeple duz plot() cagrisi kullandik.Alttaki olasilik alanini hesaplatmak icin her x,y degerini her iki Gauss pdf'e geciyoruz, ve gelen sonuclari 0.5 ile (karistirma / agirlik carpanlari / mixing coefficients) carparak topluyoruz. Elde edilen sonucun kendisi de bir dagilimdir, yani karisimin entegrali alinirsa sonuc 1 gelmelidir.import numpy as npfrom matplotlib import pyplot as pltfrom mpl_toolkits.mplot3d import Axes3Dimport scipy.statsdef norm_pdf(b,mean,cov):   k = b.shape[0]   part1 = np.exp(-0.5*k*np.log(2*np.pi))   part2 = np.power(np.linalg.det(cov),-0.5)   dev = b-mean   part3 = np.exp(-0.5*np.dot(np.dot(dev.transpose(),np.linalg.inv(cov)),dev))   dmvnorm = part1*part2*part3   return dmvnormfig = plt.figure()ax = Axes3D(fig)M = 100a = np.linspace(0.,50.,num=M)b = np.linspace(0.,50.,num=M)arr = []X = np.zeros((M, M))for i in range(M):for j in range(M):   x1 = norm_pdf(np.array([a[i],b[j]]), np.array([10,10]), np.eye(2)*10)   x2 = norm_pdf(np.array([a[i],b[j]]), np.array([40,40]), np.eye(2)*0.4)   X[i,j] = 0.5*x1 + 0.5*x2   arr.append([i, j, X[i,j]])arr = np.array(arr)ax.plot(arr[:,0], arr[:,1], arr[:,2], zs=0, zdir='z', label='zs=0, zdir=z')plt.show()




![](gauss_mix_2d.png)
