# meshgrid


meshgrid



Bu fonksiyon 2 kordinat vektoru alir ve geriye 2 kordinat matrisi dondurur. Diyelim ki 3 boyutlu bir fonksiyon hesaplatacagiz, fonksiyon alani ise x kordinati -5 ve 5 arasinda, y kordinati -3, 3 arasinda olacak. O zaman bu araliktaki her noktanin kombinasyonu bize lazim. Bu kombinasyon [-5,-3], [5.1,-3],..,[-5,-3.1] diye gidecekti.meshgrid fonksiyonunun yaptigi bu kombinasyonu rahat erisilir hale getirmekten ibaret. meshgrid cagrisindan geri gelecek X ve Y matrislerinde X[1] ve Y[1]'e baktigimizda (her iki tarafta ayni indisi kullandigimizda yani) kombinasyonlardan birini aninda alabilecegiz. Ornekx = np.arange(-5, 5, 0.1)y = np.arange(-3, 3, 0.1)xx, yy = np.meshgrid(x, y)z = np.sin(xx**2+yy**2)/(xx**2+yy**2)Not: Bu kodda meshgrid'den gelen xx ve yy uzerinde direk indis kullaniliyormus gibi gozukmuyor, fakat arka planda aslinda kullaniliyor. xx ile yy uzerinde aritmetik islemler kullanilinca, bu otomatik olarak her xx ve yy elemanin teker teker, ayni indiste olanlarinin beraber isleme sokulmasi demektir, +, -, ** gibi islemler perde arkasinda buna gore kodlanmistir.Kaynak




