# Scipy


Scipy



Matlab yerine pur acik yazilim ve Python temelli arayisimiz baglaminda surada sunulan listeden devam ediyoruz.Numpy kurduktan sonra Scipy kurmak icin resmi sitesine gidelim. Ben Sourceforge sitelerinden kaynaklari indirdim. Indirip actiktan sonra o dizindesudo python setup.py installuygulayin. Ornek kod olarak Fast Fourier Transform yapan bir ornek:from scipy import *x=r_[1:4]h1=zeros(15); h1[0]=1h2=zeros(15); h2[6]=1h3=zeros(15); h3[12]=1print convolve(h1+h2+h3,x)Sonuc[ 1.  2.  3.  0.  0.  0.  1.  2.  3.  0.  0.  0.  1.  2.  3.  0.  0.]olarak geri gelmeli.Scipy'i derlerken arka planda Fortran kodlarinin, optimizasyon, diferansiyel denklem cozme amacli yazilmis C rutinlerinin derlendigini, Lapack gibi bilimsel cevrelerde cok iyi bilinen isimlerin gectigini goreceksiniz. Bu projenin hic sakasi yok. Ciddi bir  bilimsel hesaplama araci olmaya yeminli gozukuyorlar.




