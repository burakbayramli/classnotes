# Tarayıcı, Statik Dosya Yapısı

Pek cok uygulama, eger sadece okuma gerektiren turden iseler,
tarayicinin statik json, ya da farkli turden metin hatta ikisel
(binary) dosyalarini azar azar indirip isledigi sekilde
kodlanabilir. Site arama islemini [1]'de bu sekilde kodladik. Tipik bir
okuma islemi soyle

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
yaratıldı, sonuç dosyaları parçalara ayrılmış şekilde servis
gönderildi, buradan istemci ihtiyacı olan kısımları azar azar
indirebildi ve kendi tarafında ek işlemler uygulayıp HTML olarak
sunabildi.

Daha fazla neler yapılabilir? İlginç bazı fikirler var, mesela bir
arkadaş tüm bir SQLite dosyasının pür dosya olarak paylaşılıp istemci
tarafından kısmı olarak okunduğu bir mimariyi şurada [2] anlatılıyor.

```
from requests import get

url = "https://www.rfc-editor.org/rfc/rfc2822.txt"
r = get(url)
print (r.text[:100])
```










Kaynaklar

[1] statik-web-sitesi-dinamik-arama-full-text-search.html

[2] https://phiresky.github.io/blog/2021/hosting-sqlite-databases-on-github-pages/

