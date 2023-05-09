# Quantile

R dilindeki quantile(x,p) fonksiyonu x verisi uzerinde p olasilik
rakamlarina gore o olasiliklara tekabul eden kisimlari alip rapor
eder. Mesela quantile(x, probs = c(.20, .80)) cagrisi buna bir
ornek.aa = c(1,2,3,4,5,6,7,8,9,10)quantile(aa, probs = c(.20, .80))20%
80%2.8 8.2Python uzerinde ayni isi scoreatpercentile cagrisi yapiyor.x
= numpy.array([1,2,3,4,5,6,7,8,9,10])print stats.scoreatpercentile(x,
20)print stats.scoreatpercentile(x, 80)Sonuc yine 2.8 ve 8.2 olarak
gelecek.





