# PDF Numaralari, pdftk, pdfjam, gs, mutool

PDFtk

PDF dosyalari uzerinden pek cok islem pdftk ile yapilabilir. Kurmak icin

```
sudo apt-get install pdftk
```

ya da

```
sudo snap install pdftk
```

Bazi pdftk numaralari:

Dokumanlari birlestirmek (doc1.pdf,.. istenen kadar dosya bir liste
olarak arka arkaya)

```
pdftk doc1.pdf doc2.pdf output dokuman.pdf
```

Tum sayfalari 90 deree saat yonu tersine dondurmek

```
pdftk in.pdf cat 1-endwest output out.pdf
```

Bir belgenin cift sayfalarini 90 derece saga, tek sayfalarini 90
derece sola dondurmek

```
pdftk A=doc.pdf  shuffle AevenE AoddW output out.pdf
```

Bir kitabin 100. ve 110. sayfalari arasindaki sayfalari ayri bir
dokuman olarak yazmak

```
pdftk doc.pdf cat 100-110 output out.pdf
```

1-9 sayfalarini 180 derece cevirmek, geri kalanini oldugu gibi almak

```
pdftk doc.pdf cat 1-9S 10-end output out.pdf
```

20. ve 30. sayfalar arasındaki sayfaları atlayıp, geri kalanları tutmak,

```
pdftk doc.pdf cat 1-19 30-end output parcalar.pdf
```

Bir koca.pdf içine 180. sayfadan sonra ara.pdf dosyasını sokuşturmak,

```
pdftk A=koca.pdf B=ara.pdf cat A1-180 B A181-end output output.pdf
```

2 sayfa yanyana scan edilmiş PDF dokümanlarını normal hale getirmek,
ortadan bölmek için alttaki yöntemler kullanılabilir.

İlk yöntem en kolayı, önce `apt install mupdf-tools` sonra,

```
mutool  poster -x 2  input.pdf output.pdf
```

Alttaki yöntem daha eski, ama bazıları için faydalı olabilir,

Önce pdfjam adlı program lazım, `apt-get` bunu kurar, ya da [suradan](https://github.com/rrthomas/pdfjam)
indirip `build.sh` ile kurariz, ardından

```
pdfjam -o cift.pdf --trim '14cm 0cm 0cm 0cm' --clip true --scale 1.0 dokuman.pdf
```

```
pdfjam -o tek.pdf --trim '0cm 0cm 14cm 0cm' --clip true --scale 1.0 dokuman.pdf
```

```
pdftk A=cift.pdf B=tek.pdf shuffle B A output final_dokuman.pdf
```

Bu komutların yaptığı şu; biri sağdan bir soldan olmak üzere 14 cm'lik
bölümü kesip atıyoruz (trim ve clip true), ve bu budanmış
dokümanlardan birini tek sayılı sayfalar, diğerini çift sayılı
sayfalar olarak kaydediyoruz. Sonra bir pdftk taklası atmak gerekiyor,
bu komuta iki dökümanı birleştirmesini söylüyoruz, ama tek sayfaları
bir yerden, çift sayfaları başka bir yerden almasını söylüyoruz
(shuffle B A seçeneği)

Kalite Azaltmak, Kucultmek

Bazen içinde çok fazla resim olan PDF dosyalarının daha hızlı
göstermek için, küçültmek için kalitesini azaltmak isteyebiliriz. Eğer
`ghoştscript` var ise,

```
gs -sDEVICE=pdfwrite -dPDFSETTINGS=/ebook -q -o output.pdf input.pdf
```

Python

Bazen PDF içinden tablo verisi çıkartmak gerekiyor. İşaretle-kopyala
ile bu işlem her zaman rahat olmayabiliyor. Bir Python kütüphanesi
faydalı olabilir,

```
pip install tabula-py
```

ile kurarız. Tabula PDF tablolarını Pandas DataFrame olarak döndürmeye
uğraşır. Mesela 30-35 sayfalarındaki tabloları tek bir DataFrame olarak
almak için,

```python
import tabula, pandas as pd
ps = list(range(30,35))
print (ps)
table = tabula.read_pdf("doc.pdf",pages=ps)
df = pd.concat(table,axis=0)
df.to_csv('out.csv',sep=';')
```
