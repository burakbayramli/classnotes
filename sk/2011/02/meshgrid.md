# meshgrid

Bu fonksiyon 2 kordinat vektörü alır ve geriye 2 kordinat matrisı
döndürür. Diyelim ki 3 boyutlu bir fonksiyon hesaplatacağız, fonksiyon
alanı ise x kordinatı -5 ve 5 arasında, y kordinatı -3, 3 arasında
olacak. O zaman bu aralıktaki her noktanın kombinasyonu bize lazım. Bu
kombinasyon `[-5,-3], [5.1,-3],..,[-5,-3.1]` diye gidecekti.

meşhgrid fonksiyonunun yaptığı bu kombinasyonu rahat erişilir hale
getirmekten ibaret. meşhgrid çağrısından geri gelecek X ve Y
matrislerinde `X[1]` ve `Y[1]`'e baktığımızda (her iki tarafta aynı
indisi kullandığımızda yani) kombinasyonlardan birini anında
alabileceğiz. Örnek

```
x = np.arange(-5, 5, 0.1)
y = np.arange(-3, 3, 0.1)
xx, yy = np.meshgrid(x, y)
z = np.sin(xx**2+yy**2)/(xx**2+yy**2)
```

Not: Bu kodda meşhgrid'den gelen xx ve yy üzerinde direk indis
kullanılıyormuş gibi gözükmüyor, fakat arka planda aslında
kullanılıyor. xx ile yy üzerinde aritmetik işlemler kullanılınca, bu
otomatik olarak her xx ve yy elemanın teker teker, aynı indiste
olanlarının beraber işleme sokulması demektir, +, -, ** gibi işlemler
perde arkasında buna göre kodlanmıştır.





