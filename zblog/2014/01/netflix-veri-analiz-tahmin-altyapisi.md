# Netflix Veri Analiz, Tahmin Altyapisi




Netflix Veri Analiz, Tahmin Altyapisi 




Xavier Amatriain Netflix'in yapay ogrenim altyapisindan bahsediyor.

www.infoq.com/presentations/machine-learning-netflix

Netflix Blog

Netflix bilindigi gibi web uzerinden video servisi sunan ve unlu Netflix Odulunu baslatan sirket; ugrastiklari en onemli problemlerden biri kullanicinin seyretmedigi bir filme 1-5 arasinda hangi notu verecegini tahmin etmek, yani tavsiye etme problemi (collaborative filtering). Kullanilan yaklasimlardan biri SVD temelli ki bu yaklasima benzer iki cozum bizim matematik notlarinda islendi, digeri ise RBM (restricted boltzman machines) temelli. RBM'ler ust uste konunca derin ogrenim (deep learning) oluyor, ki burada buyuk usta Geoffrey Hinton'dan bahsetmek gerekir. Netflix sistemi tek seviyeli RBM kullanmis, yani teknik olarak derin ogrenim degil, fakat RBM'lerin avantajlarini gostermesi acisindan ilginc.

YO dunyasinda baslayan bir tartismaya da ilginc bir ek yapti Amatriain; Google'da calisan yapay zeka uzmanlarindan Norvig "daha cok veri daha iyi algoritmayi yener" gibi bir yorum yapmisti. Amatriain bu ifadeye bir ek yapiyor "... eger verinin boyutlari (dimensionality) yuksek ise". Hakikaten de Norvig cogunlukla konusma dili isleme problemleri uzerinde calisiyor ve bu problemlerde evet boyutlar cok yuksektir -- dildeki her kelime bir boyut haline gelir. Amatriain daha az boyutlu bir veri uzerindeki ogrenim basarisini paylasmis, x-ekseni veri miktari, y-ekseni basari



Goruldugu gibi 2-3 milyon veri sonrasinda daha fazla veri pek fark yaratmiyor.

Konusmada siralama (ranking) icin kullanilan bir numara oldukca ilginc (prezentasyona bakiniz). Siralama problemi bilindigi gibi verilecek not tahmininden farklidir, not tahmini her / herhangi bir film icin yapilir, ama o filmlerin arasindan bir kullanici icin "en iyi 10 film" secmek biraz daha degisik bir problemdir, ki siralama yapmak bu noktada gerekir. 





![](Screenshotfrom2014-01-20101818.png)
