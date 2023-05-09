# Birim Testleri, Taklitlemek (Mocking) ve Python

Birim test bir kod parçasının beklenen sonuçları vermesini test eder,
eğer toplamayı test ediyorsak,

```python
def topla(a,b): return a+b

assert topla(2,3)==5
```

kodu yazılabilir; assert ifadesi eğer beklenen şart oluştuysa hiçbir
şey yapmaz, olmazsa hata verir. Bir kodun mantığını bu sekilde test
edebiliriz, cunku eger kodda hata varsa, bekledigimiz sonuc
verilmez. Eger ustteki + isareti - olsaydi, bir AssertionError
verilirdi.

İhtiyacı olan her şeyi kendi içinde barındıran kodlar için bu tür
testler problem değil, üstteki kod için + işleminden başkası
gerekmiyor. Fakat eğer bir kod parçasının bir dış sistem ile
bağlantıya geçmesi gerekiyorsa birim test sırasında bu bağlantıların
yerine taklit kod (mock) geçirmemiz gerekebilir. Özellikle dikkat
edilmesi gereken husus şudur; taklit, ya da gerçek kodlarından birini
atıp diğerini kullanabilmeliyiz, tasarım temiz bir şekilde olmalı.

Diyelim ki o andaki sistem zamanını alıp hangi günde olduğumuza bakan
bir kod var.  Kod eğer o anki gün iş günü ise (Pazartesi-Cuma arası)
verilen bir değere 200 ekliyor, yoksa 100 ekliyor ve döndürüyor..

```python
import datetime
def f(val):
    t = datetime.datetime.now()
    if t.weekday()==6 or t.weekday()==7: return val+100
    else: return val+200

print f(50)
```

Sonuc

```
150
```

Üstteki sonuç geldi çünkü bugün (yazının yazıldığı gün) Cumartesi. Bu
kodu birim testinden geçirmek için ne yapmalı?  Bir test kodu yazarız,
o f()'i çağırır sonucu kontrol ederiz bu çok basit fakat kod her
işlediğine değişik bir günde olabiliriz, kodun sürekli hafta sonu
işletilmesi garanti değil, o zaman beklediğimiz cevap ile verilen
cevap habire değişebilir. Birim testleri bir kere yazınca her
seferinde değiştirmek istemeyiz. Ayrıca birkaç degisik durumu,
senaryoyu da test etmemiz lazım, hafta sonu olması, hafta içi olması,
vs. bunlari testlerimiz kontrol edebilmeli.

Taklitleme burada ise yarar. Fakat test edilen kodun tasarımını öyle
yapmalıyız ki dış sisteme bizim sağladığımız bir aracı üzerinden
bağlanılsın.

```python
import datetime

def system_time():
    return datetime.datetime.now()
    
def f(val,timer=system_time):
    t = timer()
    if t.weekday()==6 or t.weekday()==7: return val+100
    else: return val+200
    
print f(50)
```

Sonuc

```
150
```

Dikkat, f() çağrısına parametre olarak bir fonksiyon geçiyoruz, Python
ile fonksiyonları parametre gibi geçmek mümkün, ki f() çağrısı kendi
içinde sistem zamanını okumak için bu dışarından verilen fonksiyonu
kullanmalı. Üstteki örnekte f()'e gerçek sistem zamanını okuyan
system_time geçtik, ayrıca bu fonksiyonu olağan (default) değer haline
getirdik böylece eski çağrı şekli (yani aracının verilmediği hal)
aynen olduğu isleyecek.

Şimdi bir taklit fonksiyonu yaratalım, ve f()'i onun üzerinden
çağıralım,

```python
def mock_1():
    return datetime.datetime(2016, 5, 26)
    
print f(50,mock_1)
```

Sonuc

```
250
```

Bu taklit fonksiyon her seferinde 26/5/2016 tarihi cevabı verecek
şekilde kodlandı. Böylece f() içinde okunan sistem zamanının ne
olacağını biz dışarıdan belirlemiş oluyoruz. Böylece girdiler ve dış
bağlantılar tamamen kontrol altında oluyor ve f()'ten buna göre
beklediğimiz çıktı değerlerini kontrol etmemiz mümkün oluyor. 250
bekliyorduk, 250 aldık. Testlerimiz her işlediginde bu taklit kodunu
kullanacaktır, ve girdiler hiç değişmeyeceği için beklenen çıktıların
kontrolü mümkün olacaktır.

Degisik senaryo demistik, simdi hafta sonu durumu için bir ayrı taklit
yazarız,

```python
def mock_2():
    return datetime.datetime(2016, 5, 29)
    
print f(50,mock_2)
```

Sonuc

```
150
```

Birim testlerin otomatik kontrolü için assert çağrısı kullanılır, eğer
assert sonrası verilen değer True değilse, assert hata ile bize
bildirir. Kontrolleri alttaki gibi yapabilirdik,

```python
assert f(50,mock_1) == 250
assert f(50,mock_2) == 150
```

Hiç hata yok. Ama kod içinde bir yanlışlık olsaydı (ya da örnek
amaçıyla baska bir değer bekliyormuş gibi yapalım),

```python
assert f(50,mock_1) == 3883
```

```
---------------------------------------------------------------------------
AssertionError                            Traceback (most recent call last)
 in ()
      1 
      2 
----> 3 assert f(50,mock_1) == 38838

AssertionError: 
```

Sonucunu görürdük.

Taklitleme ile bu şekilde otomatik testler yazabiliriz. Üstte ardı
ardına birçok kontrolü yapabilirdik, eğer hiç hata mesajı görmezsek,
kodun beklediğimiz gibi işlediğini görürdük, ve içimiz rahat ederdi.

TestCase

Ben çoğunlukla bir main program içinde assert çağrıları kullanıyorum,
yeterli oluyor. Ek olarak unittest modulu kullanılabilir.

```python
import unittest

class TestBizimClass(unittest.TestCase):

 def test_metot1(self):
     ...
     self.assertFalse ( .. )
             
 def test_metot2(self):
     ...
     self.assertFalse ( .. )
             
if __name__ == '__main__':
 unittest.main()
```

Bu script isletilince unittest.main() cagrisi her iki test metotunu
otomatik olarak cagirip, sonuclari toplar, ve Java dunyasinda JUnit,
TestNG'ye benzeyen sonuclari ekranda gosterir.

Örnek

Taklitlemede ne kadar ileri gidilebilir? Tabii ki istendiği kadar;
çoğu mühendislik durumlarında olduğu gibi bir bedel / getiri dengesi
var, her dış sistemi taklitlemek çok uğraştırabilir, fakat eğer test
edilenler kritik ise bu efora değebilir. Bizim en son bir servise
bağlanıp en son finans verilerini indiren ve bir Mongo tabanına yazan
kodu test etmemiz gerekti, bu durumda dış bağlantıyı taklitledik,
ayrıca verilerin apayrı bir tabana yazılmasını sağladık (yani bağlantı
aracı kodu, ve taban ismi hep dışarıdan fonksiyonlara
veriliyordu). Dış servisin geri getirdiği veriler tabii ki her test
şartına göre farklı olmalıydı, bunun için ayrı ayrı dizinler altında
ayrı CSV dosyaları hazırladık, ve her taklit şekli bu farklı (ama
bilinen) veri setini geri döndürüyordu, test tabanı her test başında
silinip, tekrar yaratılıyor (tabii bu tür işler anahtar-değer tabanı
Mongo ile çok kolay), vs.

Bu çok efor gibi gözükebilir, fakat bu koda 5 sene sonra dönsem python
test.py işletip her şeyin işlediğini görebilirim, bu hakikaten bir
rahatlık saglar. Birim test gurusu Kent Beck bazen kahve molası verip
sonra geri masasına tekrar oturduğunda "koda devam etmeden önce birim
testleri şöyle bir daha işletirim, her şey tamam mesajını görmek beni
rahatlatır" mealinde bir yorum bile yapmistir. Diğer taraftan şimdiye
kadar insanlığın geliştirdiği en büyük kod bazı olan Linux'ta
bildiğimiz kadarıyla hiç birim test kullanılmıyor - fakat oradaki
sebep herhalde Linux'un çok temel seviyede ve donanıma çok yakın
işliyor olması, ayrica Linux çekirdeğinin dolaylı ve dolaysız
kullanıcı tabanı o kadar büyük ki bu kullanıclar her türlü otomize
test işlemini gerçekleştirmiş oluyorlar bir bakıma.





