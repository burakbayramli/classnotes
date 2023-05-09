# imap

Python itertools paketinden bir fonksiyon daha: imap. Bu yardımcı
çağrı adından anlaşılabileceği üzere "eşleştirme" yapar. İki veya daha
fazla parametre alır, birinci parametre her zaman çağırılacak bir
fonksiyon F ölacaktır (evet fonksiyona parametre olarak fonksiyon
geçiyoruz), geri kalan parametreler içinde F'e verilecek olan
parametrelerin listesi oluyor. Mesela pow çağrısını düşünelim, bu
çağrı iki parametre alır, `pow(2,2)` mesela 2 üzeri 2 hesabını yapar,
2^2 = 4. Diyelim ki bunun gibi pek çok sayı var elimizde, üstü
alınacak sayılar bir listede, üst değerleri başka bir listede
duruyor. imap ile bu işi şöyle hallederiz:

```python
from itertools import imap

aa = [2,3,4]
bb = [2,4,6]

for x in imap(pow, aa, bb):
    print x
```

Bu çağrı 2^2, 3^4, 4^6 hesaplarını sırasıyla yapar ve sonuçların
"gezilebilir" hale getirilmesini sağlar (imap kodlamasında yield
kullanımı var yani).





