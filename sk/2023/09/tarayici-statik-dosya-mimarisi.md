# Tarayıcı ve Statik Dosya Servisi ile Uygulama 

Pek çok uygulama, eğer sadece okuma gerektiren türden iseler,
tarayıcının statik json, ya da farklı türden metin hatta ikisel
(binary) dosyalarını azar azar indirip işlediği şekilde
kodlanabilir. Site arama işlemini [1]'de bu şekilde kodladık.

Tarayıcı içinden servisteki bir dosyayı okuma işlemi şöyle,

```html
<head>
    <script defer src="pyscript.js"></script>
</head>
<body>    
      <py-script>
          from pyodide.http import open_url
          url = 'https://raw.githubusercontent.com/fomightez/pyscript_test/main/turtles.csv'
          df = open_url(url)
          display(df.getvalue())
      </py-script>
</body>
</html>
```

Tersyüz edilmiş indis önceden, arka planda, network dışında (offline)
yaratıldı, sonuç dosyaları parçalara ayrılmış şekilde serviste
paylaşıldı, buradan istemci ihtiyacı olan kısımları azar azar indirir
ve kendi tarafında işlemler uygulayıp HTML olarak sunabilir.

Daha fazla neler yapılabilir? İlginç bazı fikirler var, mesela bir
arkadaş tüm bir SQLite dosyasının pür dosya olarak paylaşılıp müşteri
tarafından kısmı olarak okunduğu bir mimariyi şurada [2] anlatılıyor.
Yani teorik olarak 1 Gıgabaytlik bir sqlite taban dosyası pür dosya olarak
paylaşılabilir, bağlanan onun gerekli kısımlarına noktasal (hızlı)
erişim yaparak veriyi alır. Sonuçta bir sürücü (driver) kodunun da
yaptığı bu değil midir? `import sqlite3` diyerek bu tür bir sürücüyü
kullanıyoruz sürekli, o işlemi network üzerinden yapmak imkansız değil.

Eğer kendi kısmı erişim kodlarımızı yazmak istersek, başlangıç noktası
Web'in "kapsam isteği" (range request) standardı olabilir. İnternet'ten
dosya indirirken belki farketmişizdir indirme işlemini bazen dondurabiliyoruz,
ve sonra devam deyince indirme kaldığı yerden devam ediyor. Ya da eğer komut
satırında `curl` ile indirim yapıyorsak, arada programı durdurup sonra
`curl --continue` ile devam edebildiğimizi de biliyoruz.  Tüm bunlar
Web'in bu kapsam isteği standardı ile mümkün oluyor. Bu tür isteği
kullanmak için istek başlığı (header) içine `range` ifadesi gömmek yeterli,

```
from requests import get
url = "https://raw.githubusercontent.com/Cheukting/pyscript-ice-cream/main/bj-products.csv"
res = get(url, headers={"range": "bytes=0-50", "Accept-Encoding": "identity"})
print (res.text)
res = get(url, headers={"range": "bytes=50-100", "Accept-Encoding": "identity"})
print (res.text)
```

Üstteki kodlar aynı dosyanın farklı yerlerini gösterecekler.

Zihin egzersizi: Bir tavsiye sistemini serviste pür dosya kullanarak
nasıl yazarız? Tavsiye sistemlerinin en basit şekli kullanıcı
yakınlığı bulmak. Tabanda 10 tane film varsa kullanıcı A belki üçünü
seyretti, not verdi, bu bize 1 x 10 vektörü sağlar vektör içeriği `[0
0 4 0 5 2 ... 0]` diye gidebilir.  Taban verisinde 500 tane farklı
seyirci olabilir, onların verdiği notlar da kayıtlıdır, yani taban 500
x 10. Kullanıcı A'ya film tavsiyesi üretmek için onun vektörünü alırım
tüm tabanı tarayıp ona en yakın kişileri bulurum ve A'nın seyretmediği
ama bu kişilerin seyredip beğendiği (4, 5 not verdiği) filmleri
tavsiye ederim. Bu kadar basit.

Rakamları büyütelim; tabanda 150,000 kişi olabilir, film sayısı
60,000. Yani sadece tek kişinin beğeni vektörü 1 x 60K olacak ve bu
vektörün 150K kişiye yakınlığı hesaplanacak! Zor iş. Javascript ->
Statik mimarisinde tarayıcıya bir şeyler göndermek lazım, ne
göndereceğiz? Tüm tabanı, yani 150K x 60K büyüklüğünde bir matrisi
gönderemeyiz. Movielens verisi bu ölçülerde, ve beğeni matrisi her ne
kadar seyrek olsa da (her kullanıcı her filmi seyretmemiş, ve o
değerler tabana dahil edilmiyor) bu hala 300 MB büyüklüğünde bir
dosya.

Tabanı parçalara bölmek için bir yapay öğrenim tekniği kullanırız,
mesela K-Means küme bulma tekniği. Geliştirme ortamında yerel dosya
üzerinde kuvvetli bir bilgisayar ile 150K x 60K matrisinin kümelerini
buluruz, diyelim K=20 olsun, K-Means bize içinde birbirine benzeyen
kişilerin olduğu 20 tane küme verir. Kümeler merkezleri üzerinden
temsil edilir, bir ortalamadır bunlar, bu örnekte ufak bir 20 x 60K
matrisidir olacaktır.

Böylece yeni kullanıcı tarayıcı ile bağlandığında ona küme
merkezlerini göndeririz, hızlı, yeni kullanıcı kendi mesafesini bu
merkezler için hesaplar, bu da hızlı işler. Sonra kullanıcı bir kümeyi
seçer, ve o küme içindeki kişilerin beğenilerini azar azar, ve kapsam
isteği ile servisteki büyük dosyadan alabilir. Bunlar ona verilecek
tavsiyeler olacaktır.

Yani biraz önceden işlem (preprocessing) yaparak ve büyük dosyaları
parçalara bölerek ve erişimi noktasal atış haline getirerek serviste
statik dosyalarla iş yapabilir olduk.

Kaynaklar

[1] statik-web-sitesi-dinamik-arama-full-text-search.html

[2] https://phiresky.github.io/blog/2021/hosting-sqlite-databases-on-github-pages/

