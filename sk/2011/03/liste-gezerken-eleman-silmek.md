# Liste Gezerken Eleman Silmek

Python ile bir listeyi gezdigimiz anda gezdigimiz listeden eleman
silersek, gezme islemi negatif sekilde etkilenmis olacaktir. Mesela
soyle bir kod dusunelim:

```python
list = [["1","1"], ["2","2"]]
for item in list:
   print item
   if ["1","1"] in list: list.remove(["1","1"])
```

Bu kod sadece ['1', '1'] sonucunu basacaktır, çünkü gezme sırasında
bir eleman (['1', '1']) silinmiştir, ve bu listeyi küçültmüştür.Eğer
silme işleminin gezme işlemini etkilememesini istiyorsak, o zaman
listenin bir "kopyası" üzerinde gezinti yapmamız lazım. Python'da
kopya çıkartmak için clone, copy gibi çağrılar yerine bir operatör
kullanılıyor; bu operatör [:] operatörü. O zaman:

```python
list = [["1","1"], ["2","2"]]
for item in list[:]:
   print item if ["1","1"] in list: list.remove(["1","1"])
```

istediğimiz sonucu verecektir.





