# Dongu Yazmamak, Fonksiyonel Diller, Python

Büyük miktarda veri işlerken "for" komutunu kullanan döngülerden
kaçınmak iyi olur. Python'un fonksiyonel formatı ve kütüphanelerin
sağladığı hizmet zaten daha kısa / temiz sekilde listeler, numpy
vektörleri, matrisleri üzerinde işlem yapılmasını sağlar. Ama daha
önemlisi, bu tek çağrılık fonksiyonel kullanımların arka planda Ç ile
işleyen kodlara direk gitmesidir. Yani hızlı işleyeceklerdir.

Mesela elimizde bir kelime listesi olsun, bu listenin her elemanının
her diğer elemanına olan Levenshtein uzaklığını hesaplayıp bir matrise
yazacağız. İlk yaklaşım hemen

```
for w1 in words:
   for  w2 in words:
```

diye bir döngü yazar. Bu çok yavaş işler çünkü döngülerin kendisi
Python içindedir. Daha iyisi itertools.product(words, words) ile
kelimelerin tüm kombinasyonunu hesaplatmaktır. Avantaj bir,
itertools.product C ile kodlanmış ve hızlı. İki, geriye döndürülen bir
öğe gezici (iteratör) ve her istek için tek eleman üretiyor, tüm
hafızayı tüm sonuç ile bir anda doldurmuyor.

Şimdi bu kombinasyon üzerinde uzaklık hesabını `itertools.imap(f, ...)`
ile işletiriz. f fonksiyonu

```
f = lambda (x,y): leven.distance(x,y)
```

olarak tanımlanır, distance daha önce bahsettiğimiz mesafe hesabı, o
da C ıle işleyen bir kodda. Bakış açısındaki değişime dikkat: döngü
her şeyi kontrol etmiyor, imap fonksiyonu, döngü ve veriyi
"eşleştiriyor", birini alıp ötekine uyguluyor.  Bu işleyince elimizde
mesafeler var, ve öğe gezici üzerinden bu hesaplar isteyene
veriliyor. Peki geziciden Numpy matrisi nasıl oluştururuz? np.fromiter
ile.

```
words = np.array(['filan', 'fisman', 'sisman', 'paspas']) 
(dim,) = words.shape
f = lambda (x,y): leven.distance(x,y)
res=np.fromiter(itertools.imap(f, itertools.product(words, words)),
                dtype=np.uint8)
A = np.reshape(res,(dim,dim))
```

Koda tekrar bakarsak, product, imap, fromiter ve leven.distance
çağrılarının hepsi C içinde işliyor. Yani hesap oldukca hızlı
olacak. Genel olarak kodlama felsefesi de şöyle (değişmeye
başladı). Python, Ruby gibi diller fonksiyonel kodlamaya izin veren,
onu özendiren diller, döngünün veri işlediği değil, fonksiyonların
döngülere parametre olarak verildiği bir yaklaşım bu. Aslına bakılırsa
modülarite açısından mantıklı. Döngüler de çoğunlukla birbirine
benzeyen ve bir kere kodlayıp bir daha kodlamak istemeyeceğiz şeyler.

Ayrıca performans açısından da veri analiz kodlarını hep "Ç içinde
tutmak" için, üstte gördüğümüz gibi, fonksiyonel tarz daha öne
çıkıyor.

Not: Gezicilere bir daha değinelim. Üstteki çağrı zincirinde sürekli
gezicilerin döndürdüğü tek eleman üzerinde işlem yapılıyor, ve
gezicilerin kodları C içinde. Eğer tek bir gezici, ufak veri olsaydı
gezici kullanımı pek bir fark yaratmayabilirdi. Ama onbinlerce öğeli
bir vektör üzerinde arka akraya bir sürü işlem uyguluyor olsaydık,
gezici olmadığı durumda her basamakta 10,000 üyeli bir vektör daha
yaratıyor olurduk. Bu hem hafıza hem CPU israfı demek olurdu.





