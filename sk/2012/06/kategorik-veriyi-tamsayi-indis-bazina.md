# Kategorik Veriyi Tamsayi (Indis) Bazina Cevirmek

Veri analizinde bazen kategorik veriler yerine pur sayi kullanmak
gerekebilir. Ozellikle regresyon islemleri tam sayilari alarak islem
yapabilirler, bu yuzden, mesela 'YAZ', 'BAHAR', gibi mevsim verisini
kelime olarak iceren veriyi tam sayi bazli, 1,2,3.. turune cevirmek
gerekebilir. Hizli bir yontem, Numpy unique cagrisina return_inverse
secenegi vermek. Bu cagri bilindigi gibi verilerin tekil bir ozetini
geri getirir,

```
a = np.array( ['a', 'b', 'c', 'a', 'b', 'c'])

print np.unique(a)
```

geriye


```
['a' 'b' 'c']
```

dondurur. Ek secenek ile indis degerlerini de alabilirsiniz,

```
b, c = np.unique(a, return_inverse=True)

print c
```

su sonucu verir

```
[0 1 2 0 1 2]
```








