# Toptan İşlemler, Paralelizasyon, Tekrar Başlatılabilirlik (Restartability)

### Paralel İşlem

IT veri işlemciliğinde çok karşılaşılan bir problem: çok sayıda bir
grup verinin sırayla işlenmesi. İşlemi paralelize edersek (eldeki her
çekirdek için bir veya daha fazla süreç başlatarak) aynı anda daha
fazla veri işleyebiliriz. Fakat paralelize etmeden önce süreçlerin
birbiriyle kordinasyonunu ayarlamak gerekli.

Bir yaklaşım her paralel süreç işlemek istedikleri kayıtı "kitlemeye"
uğraşabilirler, önce gelen kitler, sonrakiler başarısız olup sonraki
kayıdı kitlemeye uğraşırlar, vs. Bu yaklaşımı Kurumsal Java kitabında
işlemiştik. Bu tabii daha farklı bir dünyaydı, Oracle, satır bazlı
kilitler, vs. Artık anahtar/değer tabanları dünyasındayız, Mongo ile
bir kaydın anahtar üzerinden statüsünü kontrol etmek çok hızlı. Diğer
yaklaşımın da faydalı olacağı yerler olabilir muhakkak. Faydalardan
biri önceden parça sayısı (bilahere kaç sürecin paralel işleyeceği)
tanımlamaya gerek yok, ihtiyaca göre yeni bir süreç başlatılır, yeni
süreç hemen işlenmemiş kayıt / kitleme mekanizmasına dahil olur,
sıradaki kayıdı alıp işlemeye başlar. Fakat çoğunlukla önceden hangi
makinada kaç süreç işleyeceği hakkında iyi bir fikrimiz vardır, bu
durumda külfetli kitleme işlerine girmeye gerek olmayabilir.

Ama en basit olan her paralel sürecin tamamen ayrı bir veri grubu
üzerinde işlem yapması. Yani elde 100 satır var ise 4 süreç
başlatırız, her süreç 25 satır üzerinde, diğerlerinden tamamen
bağımsız bir şekilde iş yapar.

Bir IT problemi daha: kaç süreçli olursa olsun üstte bahsettiğimiz
işlem bizimle alakalı olmayan bir sebepten dolayı çökebilir. Ardından
programı tekrar başlatırsak programın o ana kadar işlediği satırları
atlayıp, işlenmemiş satırlara gitmesini isteriz, bu "kaldığımız yerden
devam" özelliği zamandan tasarruf sağlar.

Önce 1. probleme bir çözüm bulalım. Bir işleyici script düşünelim,
eğer parçalı işlem mantığını kodlayacaksak, şöyle bir komut satırı
arayuzu düşünebiliriz,

```
python isle.py 0 4
```

Bu komut script'e veriyi 4 parçaya böldürüp 0. parçayı (çünkü
kullandığımız dil 0-indis bazlı) işlemeye başlar. Unix ortamında
sonuna & koyarak script'i arka planda işletmeye başlatabilirdik,
ardından

```
python isle.py 1 4 &
```

ile hemen bir diğer parça üzerinde de işlem başlatırdık. Dikkat:
parçaya bölmek için "işlenecek şeyler" listesi hazırladık, bu liste
her süreçte aynı, o yüzden bir tarafta 1. bir tarafta 2. vs. parçasına
gidebiliyoruz, çünkü liste aynı liste, ayrı parçalarının birbiriyle
alakasının olmadığına eminiz.

Bu şekilde devam edip 4 tane aynı anda paralel işleyen süreç
başlatarak, veriyi 4 kat daha hızlı işleyebilirdik, tabii bunu derken
işlemi yapan bilgisayarda 4 veya daha fazla mikroişlemci / çekirdek
olduğunu varsayıyoruz. Ama bu yorumu biraz daha genisletelim; eğer
yaptığımız işlemler mikroişlemciyi çok meşgul tutan türden değil ise,
mesela Internet'ten bilgi indirmek (bu durumda network bekleniyor), ya
da sabit disk işlemleri (bu durumda diskin işini bitirmesini
bekleniyor, İngilizce "IO-bound" denen durum) bu durumlarda 4 süreç
için 2 tane mikroişlemci olması bile yeterli olabilir, çünkü daha iyi
performansın önündeki engel mikroişlemci değil. Fakat paralelizasyon
hala faydalı çünkü bir süreç network'u beklerken diğeri verinin kendi
parçası üzerinde ilerleme sağlayabilir.

Ikinci problemin çözümü, mesela işlenecek şeyler bir veri tabanından
geliyorlarsa, tabanda bir status kolonu yaratmak, ve işlenen veri
satırında bu statü belirten kolonu o satır için "işlendi" konumuna
getirmek. Her satır işlenmeden önce bu statüye bakılır, eğer işlenmiş
ise atlanır.

Statü irdelemesi farklı yollardan olabilir; eldeki 10 senet için
kapanış fiyatlarını güncel tutmak isteyen bir program varsa "kaldığı
yerden devam etmek" her senet için eldeki en son tarihten (mesela dün,
iki gün oncesi, vs) o günün tarihine, yani bugüne kadar olan yeni
senet fiyatlarını almaktır. Eğer eksişözlük sitesinden 100 tane sayfa
indirip bu sayfaları HTML olarak bir dizine yazıyorsam, o HTML
dosyasının dizin içinde olup olmaması bir tür statüdür; eğer orada ise
o işlem tamamlanmıştır.

Çöküş ardından tekrar başlatım için pek çok Unix bazlı takip edici
programlar var; mesela supervisord. Bu takip edici araçlar sayesinde
komut satırından elle biz program başlatmıyoruz, takip edici
programlarımızı başlatıyor, onları izliyor, çökmüs ise tekrar
başlatıyor, vs.

Paralel, Satır Bazlı Dosya İşlemek

Paralel işlem için işbölümünün nasıl yapılacağını kararlaştırmak
lazım, burada en kolay yöntem ustte bahsettigimiz gibi veriyi baştan
parçalara bölerek her sürecin kendi parçası üzerinde çalışmasını
sağlamak. Her süreç veriyi satır satır okur ve anında işler (böylece
fazla bellek sarfedilmiş olmaz), ve sonraki satıra geçer.

İkinci soru, bir koordine edici "kalfa" süreç mi iş dağıtsın, yoksa
her süreç buna kendisi mi karar versin?" İkinci seçenek daha kolay; N
parça içinden her sürece P parçası üzerinde çalışacağını söyleriz,
süreç gerekli yere giderek işlemi yapar. Metin bazlı dosya (CSV)
işlerken parçalar ayrı satırlardır. 10 satırlı bir dosyanin ilk 5
satırını bir süreç ikinci 5 satırını başka bir süreç işleyebilir.

Basit olan yaklaşımlar daha rahat ölçeklenebilir, bu sebeple üstte
tarif edilen yaklaşım hiçbir şey paylaşmayan (share nothing)
mimarisidir, çok büyük olan girdi dosyasına tüm çekirdekler,
makinaların hızlı erisebileceği farz edilir, işbölümü veri bazlıdır
tüm süreçler hangi parçanın kendilerinde olduğunu işe başlamadan
öğrenirler, ardından iletişim olmaz. Böylece işlem sırasında fazla
makinalararası iletişimle ortaya çıkabilecek hataların önüne geçilir,
ve kodlama rahatlaşır, işlem hızlanır.

Fakat temel teknik bir problem var, CSV kütüphaneleri satır bazlı
ileri zıplama yapamaz, bayt bazlı zıplama yaparlar. Mesela 1200
baytlik dosyanin 125'inci baytina (karakterine) git diyebiliyoruz, ve
bu hızlı oluyor, fakat bu işlemin herhangi bir satırın başına gitme
garantisi tabii ki yok. O zaman biraz takla atarak her parça için
hızla gittiğimiz bayta en yakın satır başına gidebilmemiz lazım,
böylece oradan satır satır işlem yapabilelim.

Bir dosya üzerinde görelim,

```python
!cat in2.csv
```

```text
1,11111111
2,22222222
3,33333333
4,44444444
5,55555555
6,66666666
7,77777777
8,88888888
9,99999999
10,1010101
11,1111111
12,1212121
13,1311313
14,1414141
```

Bu dosyayı 2 parçaya bölelim, 

```python
import os
file_name = 'in2.csv'
N = 2
file_size = os.path.getsize(file_name)
print ('tum =', file_size, 'her parca =', file_size / N)
```

```text
tum = 154 her parca = 77.0
```

Şimdi `seek` ile ikinci parçaya gidebiliriz, ve oradan birkac
karakteri okuyalım,

```python
with open(file_name, 'r') as f:
    f.seek(78)
    c = f.read(5)
    print (c)
f.close()
```

```text
,8888
```

`8,88888888` satırında bir yerlere geldik yani. Ama bize satır başı
lazım, bir numara yapalım, eğer `seek` sonrası `readline` dersek,
içinde olduğumüz satırı bitirip bir sonrakine geçmiş oluyoruz,

```python
with open(file_name, 'r') as f:
    f.seek(78)
    f.readline()
    c = f.read(5) # bir sonraki satirdayiz simdi
    print (c)
    print ('bayt noktasi', f.tell())
f.close()
```

```text
9,999
bayt noktasi 93
```

Evet gördüğümüz gibi 8'li satırı geçtik 9'lu satırın başına gelmiş olduk,
bu bayt olarak 93'uncu karaktermiş.

Bu teknikleri daha organize şekilde bir araya koyabiliriz. Öyle bir
yardımcı fonksiyon lazim ki,

- Bir dosya ismi ve parça (chunk) sayısı alınca her parçanın
  *satırsal* başlangıç bayt noktalarını bir liste olarak hesaplasın.
  
- İşleyici kod parça bilincine sahip olsun, bir parçanın satırsal
  başlangıcına atladıktan sonra, o parçanın bitiş noktasından daha
  ileri gitmesin.

Bu yardımcıyı daha da süsleyebiliriz, farklı şekillerde kullanabilmek
için biraz daha genelleştirelim. Mesela "beni arama ben seni ararım"
tekniği ile yardımcı fonksiyon "çengel çağrılar" üzerinden her satırı
işlemek için bir dış fonksiyonu çağırabilir, böylece isteğe uygun
(custom) kodlama kolaylaşır. Parçalara ayırma, zıplama vs işlemleri
halledildi, fakat her uygulamanın satır işleyişi farklı, orada
dışarıdan tanımlı tek bir özel fonksiyon bu işleri yapabilir, altyapı
kodunun her seferinde değişmesine gerek kalmaz. O zaman islemciye bir
dış obje verelim, orada

- Her satırın nasıl işleneceği obje üzerindeki `exec` çağrısı içinde
  tanımlanır, işleyici altyapı her satır için bu kodu çağırır.

- Dış objenin kendi hafızası vardır, burada her satırın ekleme
  yapabileceği veri yapıları tutulabilir. Mesela her dosya satırındaki
  bilginin işlenmesi bir iç matrisin verisine ek yapabilir, parça
  bitince o matris içindeki birikmiş bilgiyi objeden alabiliriz.

- Parçanın işlemesi bittikten sonra dış objenin `post` denebilecek bir
  diğer çengeli çağırılır, burada son rötuşları koyma, temizlik yapma
  vs gibi kodlar olabilir, ne olacağına kullanıcı karar verecektir,
  altyapı çengellerde ne olduğu ile ilgilenmez (altyapı aranmıyor, o
  arıyor).

Şöyle bir kod yazabiliriz,

```python
import os

def process(file_name,N,hookobj):
    file_size = os.path.getsize(file_name)
    beg = 0
    chunks = []
    for i in range(N):
        with open(file_name, 'r') as f:
            s = int((file_size / N)*(i+1))
            f.seek(s)
            f.readline()
            end_chunk = f.tell()-1
            chunks.append([beg,end_chunk])
            f.close()
        beg = end_chunk+1
    c = chunks[obj.ci]
    with open(file_name, 'r') as f:
        f.seek(c[0])
        while True:
            line = f.readline()
            hookobj.exec(line)
            if f.tell() > c[1]: break
        f.close()
        hookobj.post()
```

Dışarıdan tanımlanacak obje şu işlemleri yapsın; `exec` içinde verilen
her satırı alıp noktasal bazlı `split` ile ayırsın, ve bu ufak
parçaları bir iç listeye eklesin. Tüm işlemler bitince `post` içinde
tüm iç listeyi ekrana bassın.

```python
class SimpleJob:
    def __init__(self,ci):
        self.res = []
	self.ci = ci
    def exec(self,line):
        tok = line.split(',')
        self.res.append(tok)
    def post(self):
        print (self.res)

# iki parca icinden ilkini isle
process(file_name='in2.csv', N=2, hookobj = SimpleJob(ci=0))
```

```text
[['1', '11111111\n'], ['2', '22222222\n'], ['3', '33333333\n'], ['4', '44444444\n'], ['5', '55555555\n'], ['6', '66666666\n'], ['7', '77777777\n'], ['8', '88888888\n']]
```

```python
# iki parca icinden ikincisini isle
process(file_name='in2.csv', N=2, hookobj = SimpleJob(ci=1))
```

```text
[['9', '99999999\n'], ['10', '1010101\n'], ['11', '1111111\n'], ['12', '1212121\n'], ['13', '1311313\n'], ['14', '1414141\n']]
```

Güzel oldu. Dikkat edersek `process` çağrısını tek makinadaki farklı
süreçlerden, hatta tamamen ayrı bir makinadaki bir süreçten
çağırabilirdik. A makinasında `0 3`, `1 3` deriz mesela, B makinasında
`2 3`.  Aynı girdi dosyası her makinada olmalı muhakkak ama bu kolay,
dosya kopyalanır, ya da disk paylaşım teknikleri ile erişim yapılır.
Parça hesabı, ileri zıplama tekniği her kod içinde ayrı işletilebildiği için
bir merkezi kordinasyon mekanizmasına ihtiyaç yoktur.

Demo amaçlı olarak her parçayı şimdi ayrı bir Thread üzerinde işletelim,

```python
import threading

thread1 = threading.Thread(target=process, args=('in2.csv', 2, SimpleJob(0)))
thread2 = threading.Thread(target=process, args=('in2.csv', 2, SimpleJob(1)))

thread1.start()
thread2.start()
```

```text
[['1', '11111111\n'], ['2', '22222222\n'], ['3', '33333333\n'], ['4', '44444444\n'], ['5', '55555555\n'], ['6', '66666666\n'], ['7', '77777777\n'], ['8', '88888888\n']]
[['9', '99999999\n'], ['10', '1010101\n'], ['11', '1111111\n'], ['12', '1212121\n'], ['13', '1311313\n'], ['14', '1414141\n']]
```

Eğer Thread yerine farklı süreçlerde [2] işlem yapılsın istersek,

```python
from multiprocessing import Process

p1 = Process(target=process, args=('in2.csv', 2, SimpleJob(0)))
p1 = Process(target=process, args=('in2.csv', 2, SimpleJob(1)))
p1.start()
p2.start()
```

Sonuç iki üstteki ile aynı olacak. Girdi dosyası ufak olduğu için
herşey çok hızlı işliyor, ama eğer süreçlerin yaratıldığını
gözlemlemek istersek, kod içine `time.sleep` ekleyip işleyiciyi suni
şekile yavaşlatabiliriz, ve bu arada işletim sisteminde geçip komut
satırında `ps eaf` ya da `htop` ile yaratılan süreçleri görebiliriz.

Altyapı kodu üzerinde bazı süslemeler yapılabilir, mesela `split` ile
alınan kolonlara indis yerine isim bazlı erişim yapmak istersek bu
özelliği altyapı seviyesinde ekleyebiliriz. Ya da dosyanin ilk K
satırı atlansın istersek bu farklı bir özellik olabilir. 

Üstteki kodların en güncel son hali [3]'te olacak.

Parça parça işleme hakkında bir örnek [1]'de bulunabilir.

Parçalar farklı makinalarda da işleyebilir, bu durumda scriptleme
işlerinin kolay yapılabilmesi için şifresiz `ssh` girişi her makinada
ayarlanırsa faydalı olur [4].

Not: Eğer girdi dosyalarının başında kolon isimleri var ise, bu
satırları atlamak gerekir. Bu tür dosyalar için bir satır atlama
kodunu `process` fonksiyonuna ekleyebiliriz. Bir `skip` parametresi
kaç satır atlayacağımızı alır, ve üstteki koddaki `beg=0` yerine

```python
with open(file_name, 'r') as f:
    for j in range(skip): f.readline()
    beg = f.tell()
    f.close()
```

ifadesi eklenebilir. Böylece her işlemci süreç `skip` kadar satırı o dosya
için atlaması gerektiğini bilir ve başlangıç doğru noktaya getirilmiş olur.

### Tekrar Başlatılabilirlik (Restartability)

Eğer SQL bazlı veri tabanındaki kayıtları işliyorsam en basit yaklaşım
işlenen satırlar üzerinde bir statü kolonu koymak, ve süreç
başladığında 'işlenmemiş satırlar' statüsündeki satırları almak, ve o
satır işlendikten sonra onu 'işlendi' statüsüne getirmek. Bu yaklaşım
daha önce bahsedilen 'satır kitleme' yaklaşımıyla uyumlu, ben işlerken
diğer süreçler bu satırı kitlemeyecek, bir sonraki satıra gidecekler,
ben o sırada işimi yapacağım. İşim bitince statüyü 'işlendi' haline
getireceğim, eh zaten kilit bende, ve bu şekilde elimdeki bir sonraki
satıra bakacağım. Eğer süreci ikinci kez başlatıyorsam ben ya da
benden sonraki süreç statüyü kitler, ve yine işlenmemiş olan satırları
alır, ve işe devam eder.

Peki tek süreçli ve mesela CSV bazlı işlem yapıyorsak yaklaşım ne
olmalı? Mesela `in.csv` içindeki satırları işleyip bir `out.csv`
üreteceğim, bir Web sitesinde sorgulama yapıp yeni bir kolon
ekleyeceğim belki, ve İnternet bağlantısı düşebilir, pek çok şey
olabilir, tekrar başlattığımda kaldığım yerden devam etmek istiyorum.

Burada en kolay yaklaşım şudur, eğer girdi verisinde baştan sonra
doğru artan bir ID, kimlik satırı varsa (ki yoksa biz ekleyebiliriz)
her süreç başlangıcında çıktıdaki en son işlenmiş kimliği alırız,
yoksa sıfır kabul ederiz, ve girdide bu son kimlikten büyük olan
satırları alıp teker teker işleriz.

Bu numaranın Python, CSV bazlı işlemesi için kritik hareketler şunlar,
işlenen her satır çıktı mesela `write` yazıldıktan sonra muhakkak
`flush` ile diske yazımı zorlanmalı, ve çıktı dosyası (ilk başlatım
sonrası) her defasında `w` ile değil `a` ile açılmalı. Böylece, ilk
hareketle, süreç patlarsa o yazılan satırı kurtarmış oluruz, ve bir
sonraki işletim ondan sonraki satıra geçebilir, ikinci hareketle çıktı
dosyasına ek yapmış oluruz, önceki sonuçları ezmeyiz.

Örnek üzerinde görelim, standart Pandas bazlı satır satır işlemi kalıbı,

```python
import pandas as pd
df = pd.read_csv('in1.csv')
for idx,row in df.iterrows():
    print (row['id'],row['name'],row['value'])
```

```text
1 n1 30
2 n2 10
3 n3 33
4 n4 8
5 nx 39
6 nu 57
7 ne 22
8 na 12
9 nn 31
10 ni 1
11 n18 2
```

Oldukça basmakalıp. Bu tema ile bir script şöyle olabilir,

```python
import numpy as np, time, pandas as pd, sys, os

def ext_proc(x):
    # dis web sitesi baglantisi bu olsun, degil ama
    # yapay kodlarla oymus gibi yapalim
    time.sleep(np.random.rand()) # suni bekleme ekledik
    return x + np.random.rand() # rasgele deger dondur

infile = "in1.csv"; outfile = "/tmp/out.csv"

if len(sys.argv)>1 and sys.argv[1] == "init":
    os.remove(outfile)
    fout = open(outfile,"w")
    fout.write("id,name,value,newval\n")
    fout.close()    

dfi = pd.read_csv(infile)
dfo = pd.read_csv(outfile)
if len(dfo) > 0: # ilk baslatimda cikti bos, onu kontrol et
    dfi = dfi[dfi.id > dfo.id.max()]
print (dfi)

fout = open(outfile,"a")
for idx,row in dfi.iterrows():
    newval = ext_proc(row['value'])
    line = "%s,%s,%s,%s\n" % (row['id'],row['name'],row['value'],newval)
    print (line)
    fout.write(line)
    fout.flush()
fout.close()
```

İlk başlatımda `init` seçeneği ile işletiriz, böylece `out.csv` düzgün
şekilde yaratılır. İşlem olurken programı yarıda keselim, sonra `init`
olmadan tekrar başlatalım. Bu başlatımın kalınan yerden devam ettiğini
göreceğiz, yeni işlenen satırlar çıktının sonuna eklenecek.

Dikkat edersek `dfi[dfi.id > dfo.id.max()]` filtresi ile girdi verisinin
çıktı verisindeki en büyük id'den daha büyük olanlarını almış oluyoruz,
böylece o satırlar işlenmiyor, ve çıktıya sürekli ekler yapılıyor.

Üstte gösterilen yöntemlerden sadece biri. En son işlenen satırın
kimliğini her satırı işlerken ufak bir dosyaya da yazabilirdik, sonra
süreç başlangıcında onu okuyup devam kalan yerden devam ederdik. Devam
etme yöntemi `DataFrame` filtrelemesi yerine `csv` paketinde bir sonrakine
atlama üzerinden olabilirdi, seçenekler muhtelif.

### Loglama

Bir takipçi tarafından, paralel şekilde program işletiliyorsa, artık print ile ekrana mesaj basmak yeterli değil, loglama gerekli, yani bir log dosyasına çıktıları güzel bir format ile yazmak. Python'da logging kütüphanesi bu işi çok rahat yapar; script başında import logging ile dahil ederiz, ve örneğimiz için alttaki kodu ekleriz,

```
fmt = '%(asctime)s - %(message)s - %(pathname)s'
fout = '/tmp/hava-%d.log' % int(sys.argv[1])
logging.basicConfig(filename=fout, level=logging.DEBUG, format=fmt)  
```

Artık logging.debug(...) ile yazılan her mesaj o sürecin kendisine ait
log dosyasına yazılacaktır, mesela 0. parça için bu /tmp/hava-0.log
olacak. Eger mesajlar direk ekrana basilsin istiyorsak filename yerine
stream=sys.stdout kullanabiliriz.


Kaynaklar

[1] [Hava Verisi Islemek](hava-verisi-islemek.html)

[2] [Ne Zaman Thread Ne Zaman Süreç?](thread-process-surec.html)

[4] [Bir Makinaya SSH ile Şifresiz Giriş](../../2005/10/bir-makinaya-ssh-ile-sifresiz-giris.html)
