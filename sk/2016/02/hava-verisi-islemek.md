# Hava Verisi Islemek

Bir örnek olarak bir meteoroloji sitesinden herhangi bir şehir için
hava durumunu indirme programını görelim. Mesela İstanbul için hava
son sıcaklığı alttaki program ile ekrana basabiliriz,

```
import re, requests
from urllib import urlretrieve

url = "http://www.mgm.gov.tr/tahmin/il-ve-ilceler.aspx?m=istanbul"
response = requests.get(url)
response_body = response.content

regex = "sondrm.*?renkMax.*?(\d+).*?C</em>"
res = re.findall(regex, response_body, re.DOTALL)
print res[0]
```

Diyelim ki elimizde bir şehir listesi var. Bu listeyi teker teker
işleyebilirdik. Üstteki programı modülarize ederdik, listedeki her
şehir için bir fonksiyon çağırıp o şehrin verisini alırdık. Fakat
üstte tarif edildiği gibi bu işlemi nasıl paralelize ederdik?

Şöyle

```
import re, os, requests, sys
from urllib import urlretrieve

sehirler = ['bursa','istanbul','afyon','rize','ordu','antalya']
base_dir = '/tmp'

def get_hava(sehir):
    url = "http://www.mgm.gov.tr/tahmin/il-ve-ilceler.aspx?m=%s" % sehir
    response = requests.get(url)
    response_body = response.content
    regex = "sondrm.*?renkMax.*?(\d+).*?C</em>"
    res = re.findall(regex, response_body, re.DOTALL)
    return res

def chunks(l, n):
    for i in xrange(0, len(l), n):
        yield l[i:i+n]

def process(liste):
    for sehir in liste:
        # bu sehir alinmis, devam et
        f = base_dir + "/" + sehir
        if os.path.isfile(f): continue
        # alinmamis, indir
        hava = get_hava(sehir)[0]
        fout = open(f, "w")
        fout.write(str(hava))
        print sehir, hava
        fout.close()

if __name__ == "__main__":

    nth = int(sys.argv[1])
    n_pieces = int(sys.argv[2])

    # bu takla lazim cunku chunks her parca icindeki
    # oge sayisini baz aliyor
    elems =  int(len(sehirler) / n_pieces)
    liste = list(chunks(sehirler, elems))
    process(liste[nth])
```

Artık

```
python hava.py 0 2
python hava.py 1 2
```

ile şehir listesini 2 parçaya bölüp her iki parçayı ayrı ayrı
işleyebileceğiz. Her şehir için hava durumu /tmp altına yazılır, eğer
o şehir /tmp altında var ise, atlanır. Böylece kaldığımız yerden devam
edebiliriz. Mesela birinci programı işletince,

```
bursa 14
istanbul 11
afyon 19
```

gelir. Şimdi /tmp altına gidip afyon dosyasını silelim, ve tekrar
işletelim. Sadece o şehrin indirildiğini göreceğiz.  Böylelikle
paralelizasyon, çöküşten kurtulma gibi problemlerin hepsini çözmüş
oluyoruz.

Eğer bu iki paralel programı dand takibinde başlatmak isterek, bir
hava.conf dosyası yaratırız, içinde

```
hava0: 
   exec: python hava.py 0 2
   restart: forever
hava1: 
   exec: python hava.py 1 2
   restart: forever
```

ve işletmek için

```
python [DAND DIZINI]/dand.py hava.conf
```

Böylece her programımız ayrı süreçler içinde, paralel olarak işletilecektir.

Not:

Örnekte meteoroloji sitesinin tüm HTML'i içinden sadece sıcaklık
verisini çekip çıkarttık; yani bir Web kazıma (scraping) işlemi
yaptık.  Kazıma güzel bir isim, sanki duvardan boya kazıyormuş gibi,
HTML'i kazıyıp içindeki veriyi ortaya çıkartıyoruz. Bu işlem için
düzenli ifadeler (regular expression, regex) kullandık. Tüm yazılım
mühendislerine öğrenmelerini şiddetle tavsiye ettiğimiz bir teknik.

Kaynaklar

[1] https://github.com/burakbayramli/kod/tree/master/dand



