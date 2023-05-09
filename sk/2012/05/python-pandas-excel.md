# Python, Pandas, Excel

## Pandas

Pandas ile CSV dosyası `read_csv` ile okunur biliyoruz. Excel
dosyalarını okumak bir o kadar kolay, `read_excel`. Eger basit bir
xls dosyasi varsa elimizde bu cagri hemen bir Pandas Dataframe'i
geri dondurecektir. 

Bir püf noktası şu durumda var; eğer Excel dosyası birden fazla
sayfalardan (sheet) oluşuyorsa, ki her sayfa bildiğimiz gibi ayrı
birer tablo olabilir, nasıl okuma yapacağız? Bu durumda, mesela
`dosya.xlsx` içindeki `Sheet1.xls` sayfasını okumak için

```python
xls = pd.ExcelFile('dosya.xlsx')
df = pd.read_excel(xls, 'Sheet1.xls') 
```

gerekir.

## İşlem Anında Excel Dosyası Yaratmak

Gerekli paketler surada listeli:

http://www.python-excel.org/

Biz Excel uretimi icin xlwt kullandik. En basit kod

```python
from tempfile import TemporaryFile
from xlwt import Workbook
book = Workbook()
sheet1 = book.add_sheet('Sheet 1')
sheet1.write(0,0,'A1')
book.save('simple.xls')
book.save(TemporaryFile())
```

0,0 hücresin bir şeyler yazılıp kaydedildi. Stil uygulaması için
easyxf tavsiye edilir, mesela `write(0,0,'vs',easyxf('alignment: horizontal right'))`
gibi.

Paketin bazı eksikleri sadece yazmaya yönelik olması, bir hücrenin
mevcut durumunu okuyup bir şeyler eklemek imkansız. Okuma için diğer
paket xlrd kullanılıyor, o zaman da ta  en baştan bir xls dosyası
açmış oluyorsunuz, ve başka bir ortamda / mod içinde oluyorsunuz.Excel
üreten kodlarda içerik dinamik olduğu zaman kör bir şekilde veriyi
uygun yere atmakla uğraşırız, birkaç kolonu satırı kapsayacak stil
uygulaması (mesela renk, ağırlık -bold-, font büyüklüğü) sonradan
belli alanlara uygulamak tercihimiz. xlwt ile bunları yapmak zor,
write ile tekrar aynı hücreye yazdığınızda o hücrenin eski değeri
eziliyor.

Çözüm `write()` metotunu kendi write metotumuz sarmalayıp (wrap), bu
bizim metot içinde global bir stil listesini her yazım için kontrol
etmek, yani stili sürekli / her `x,y` değeri için bu listeden
almak. Bu stil listesi basit bir Python listesi olabilir, yazım
başlamadan önce hazır olmalıdır, `(x1,x2,y1,y2,stil)` tüple listesi
şeklinde, stiller belli bloklar için tanımlandığı için bir dikdörtgen
içindeler, x1,x2 kullanımı bunun için. Sarmalanan write sürekli
kendisine verilen x,y'nin hangi stil dikdörtgeninin içine düştüğünü
kontrol edecek yani.

## Excel, CSV, Python

İçinde pür text verisi olan Excel dosyasını CSV dosyasına çevirmek
için iyi bir kod

https://github.com/dilshod/xlsx2csv

Hızlı işliyor, kurmaya bile gerek yok. İndirilen py dosyası direk
işletilir,

`python xlsx2csv.py dosya.xlsx dosya.csv`

şeklinde. Eger ciktida belli bir ayrac (delimiter) tanimlamak
istersek, mesela `|` isareti olabilir, o zaman

`python xlsx2csv.py -d "|" dosya.xlsx dosya.csv`

komutu kullanilabilir.













