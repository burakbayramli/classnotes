# Dizin Karsilastirmak, Uyan Elementleri Bulmak

R dilindeki match() cagrisinin karsiligi Numpy'da nedir? Mesela:a =
c(1,2,5,4,3)b = c(2,3)print (match(b,a))Bu cagrida a icinde b
elementlerine uyan ogelerin 'indis' degerleri geri
getirilir. Sonuc:[1] 2 5Python ile sunlari yapabiliriz:import numpy as
npa = np.array([1,2,5,4,3])b = np.array([2,3])match = np.in1d(a,
b)Eger in1d cagrisinin 'match' sonucunu ekrana basarsak,[False True
False False True]Bu dizinde 'a' icinde 'b' ile uyumun oldugu yerler
'True' digerleri 'False' ile isaretli. Simdi indis degerlerini
istiyorsak, ufak bir numara ile bunu hallederiz. np.arange(0,len(a))
ile bir 'indis dizini' yaratiriz, 0,1,...,N diye gider (N = len(a)),
onun uzerinde in1d()'den gelen sonucu 'filtre' olarak kullanirsak,
gerekli indis degerlerini elde ederiz.print
np.arange(0,len(a))[match]Sonuc[1 4]Not: Python indis baslangici
olarak 0 aliyor (C gibi), R 1 aliyor, 1,4 ile 2,5 arasindaki bir fark
ondan.





